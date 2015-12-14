;#lang racket

(require "render.rkt"
         "ring.rkt")

(define window-width 224)
(define window-height 256)

(define unit-width  1/224)
(define unit-height 1/256)

(define rocket-width  (* 13 unit-width))
(define rocket-height (*  8 unit-height))
(define bullet-width  (*  1 unit-width))
(define bullet-height (*  7 unit-height))

(define rocket-speed 15)
(define bullet-speed 15)
(define alien-speed  15)
(define bullet-limit 20)
(define reload-timer 1000)

(define aliens/column 5)
(define aliens/row 11)
(define alien-width (* 12 unit-width))
(define alien-height (* 8 unit-height))

(define (alien-adt x y id)
  (let* ((alive? #t)
         (kill!
           (lambda ()
             (set! alive? #f)
             100))
         (move!
           (lambda (direction)
             (when alive?
             (case direction
               ((left)  (set! x (- x unit-width)))
               ((right) (set! x (+ x unit-width)))
               ((down)  (set! y (+ y alien-height)))))))
         (draw!
           (lambda (render)
             ((render 'draw!) id x y)
             ((render 'animate!) id)))
         (dispatch-alien
           (lambda (msg . opt)
             (case msg
               ((id) id)
               ((x) x)
               ((y) y)
               ((alive?) alive?)
               ((kill!) kill!)
               ((move!) move!)
               ((draw!) draw!)))))
    dispatch-alien))

(define (alien-row type x y make-id)
  (let* ((alive? #t)
         (aliens
           (build-vector
             aliens/row
             (lambda (i)
               (alien-adt (+ x (* 4/3 i alien-width)) y (make-id type)))))
         (alien
           (lambda (idx)
             (vector-ref aliens idx)))
         (first-alien
           (let ((idx 0))
             (lambda ()
               (unless ((alien idx) 'alive?)
                 (let loop ((i (+ idx 1)))
                   (cond (((alien i) 'alive?)
                          (set! idx i))
                         ((= i aliens/row)
                          (set! alive? #f))
                         (else
                          (loop (+ i 1))))))
                 ((alien idx) 'x))))
         (last-alien
           (let ((idx (- aliens/row 1)))
             (lambda ()
               (unless ((alien idx) 'alive?)
                 (let loop ((i (- idx 1)))
                   (cond (((alien i) 'alive?)
                          (set! idx i))
                         ((= i 0)
                          (set! alive? #f))
                         (else
                           (loop (- i 1))))))
               (+ alien-width ((alien idx) 'x)))))
         (shot!
           (lambda (b-x)
             (let bsearch ((i 0) (j 10))
               (if (> i j)
                 #f
                 (let* ((aln (alien (+ i (quotient (- j i) 2))))
                        (a-x1 (aln 'x))
                        (a-x2 (+ a-x1 alien-width)))
                   (cond ((< b-x a-x1)
                          (bsearch i (- j (quotient (- j i) 2) 1)))
                         ((> b-x a-x2)
                          (bsearch (+ i (quotient (- j i) 2)) j))
                         ((aln 'alive?)
                          ((aln 'kill!)))
                         (else #f)))))))
         (move!
           (let ((direction 'right))
             (lambda ()
               (cond
                 ((and (<= (first-alien) 0)
                       (eq? direction 'left))
                  (set! direction 'right)
                  (vector-map (lambda (a) ((a 'move!) 'down)) aliens))
                 ((and (>= (last-alien) 1)
                       (eq? direction 'right))
                  (set! direction 'left)
                  (vector-map (lambda (a) ((a 'move!) 'down)) aliens))
                 (else
                  (vector-map (lambda (a) ((a 'move!) direction)) aliens))))))
         (draw!
           (lambda (render)
             (vector-map (lambda (a) ((a 'draw!) render)) aliens)))
         (dispatch
           (lambda (msg)
             (case msg
               ((y) y)
               ((alive?) alive?)
               ((shot!) shot!)
               ((move!) move!)
               ((draw!) draw!)))))
    dispatch))

(define (swarm-adt make-id)
  (let* ((x 0)
         (y 0)
         (aliens (vector (alien-row 1 x (* 9 alien-height) make-id)
                         (alien-row 1 x (* 7 alien-height) make-id)
                         (alien-row 2 x (* 5 alien-height) make-id)
                         (alien-row 2 x (* 3 alien-height) make-id)
                         (alien-row 3 x (* 1 alien-height) make-id)))
         (top 4)
         (bottom 0)
      ;   (x-bounds
      ;     (lambda ()
      ;       (unless ((vector-ref aliens bottom) 'alive?)
      ;         (do ((i (+ bottom 1) (+ i 1)))
      ;           (((vector-ref aliens i) 'alive?) (set! bottom i))))
      ;       (unless ((vector-ref aliens top) 'alive?)
      ;         (do ((i (- top 1) (- i 1)))
      ;           (((vector-ref aliens i) 'alive?) (set! top i))))
      ;       (cons (+ ((vector-ref aliens top) 'x) alien-height)
      ;             (+ ((vector-ref aliens bottom 'x) alien-height)))))
      ;   (y-bounds
      ;     (lambda ()
      ;     (let loop ((i bottom) (left 1) (right 0))
      ;       (let ((left-bound 0); ((vector-ref aliens i) 'left-bound))
      ;             (right-bound 10)); ((vector-ref aliens i) 'right-bound)))
      ;         (if (= i top)
      ;           (cons (min left left-bound)
      ;                 (+ (max right right-bound) alien-width))
      ;           (loop (+ i 1) (min left left-bound) (max right right-bound)))))))
         (shot!
           (lambda (s-x s-y)
             (let loop ((i bottom))
               (let* ((a-row (vector-ref aliens i))
                      (a-y (a-row 'y)))
                 (cond ((and (>= s-y a-y)
                             (<= s-y (+ a-y alien-height)))
                        ((a-row 'shot!) x))
                       ((= i top) #f)
                       (else (loop (+ i 1))))))))
         (move!
           (lambda ()
             (vector-map (lambda (row) ((row 'move!))) aliens)))
         (draw!
           (lambda (render)
             (vector-map (lambda (row) ((row 'draw!) render)) aliens)))
         (dispatch
           (lambda (msg)
             (case msg
               ((x-bounds) (cons 0 1));(x-bounds))
               ((y-bounds) (cons 1 0));(y-bounds))
               ((shot!) shot!)
               ((move!) move!)
               ((draw!) draw!)))))
    dispatch))

(define (rocket-adt make-id)
  (let* ((id (make-id))
         (pos-x (- 1/2 (/ rocket-width 2)))
         (pos-y 9/10)
         (direction 0)
         (set-x!
           (lambda (x)
             (set! pos-x x)))
         (set-y!
           (lambda (y)
             (set! pos-y y)))
         (direction!
           (let ((left?  #f)
                 (right? #f))
             (lambda (key pressed?)
               (case key
                 ((left)  (set! left?  pressed?))
                 ((right) (set! right? pressed?)))
               (set! direction (+ (if left? -1 0)
                                  (if right? 1 0))))))
         (move!
           (lambda ()
             (set-x! (+ pos-x (* direction unit-width)))))
         (draw!
           (lambda (render)
             ((render 'draw!) id pos-x pos-y)))
         (dispatch-rocket
           (lambda (msg)
             (case msg
               ((id) id)
               ((pos-x) pos-x)
               ((pos-y) pos-y)
               ((direction!) direction!)
               ((move!) move!)
               ((draw!) draw!)
               (else (error "unknown rocket-adt command:" msg))))))
    dispatch-rocket))

(define (bullet-adt make-id)
  (let* ((id (make-id))
         (x 1)
         (y 0)
         (exploded? #t)
         (reset!
           (lambda (new-x new-y)
             (set! exploded? #f)
             (set! x new-x)
             (set! y new-y)))
         (explode!
           (lambda ()
             (set! exploded? #t)))
         (move!
           (lambda ()
             (unless exploded?
               (set! y (- y unit-height)))))
         (draw!
           (lambda (render)
               ((render 'draw!) id x y)))
         (dispatch-bullet
           (lambda (msg)
             (case msg
               ((id) id)
               ((x) x)
               ((y) y)
               ((reset!) reset!)
               ((exploded?) exploded?)
               ((explode!) explode!)
               ((move!) move!)
               ((draw!) draw!)))))
    dispatch-bullet))

(define (bullets-adt make-id)
  (let* ((bullets (build-ring bullet-limit (lambda (_) (bullet-adt make-id))))
         (bullet-for-each
           (lambda (proc)
             (ring-for-each (lambda (b) (unless (b 'exploded?) (proc b))) bullets)))
         (shoot!
           (lambda (x y)
             (ring-next! bullets)
             (let ((bullet (ring-head bullets)))
               ((bullet 'reset!) x y))))
         (move!
           (lambda ()
             (ring-for-each (lambda (b) ((b 'move!))) bullets)))
         (draw!
           (lambda (render)
             (ring-for-each (lambda (b) ((b 'draw!) render)) bullets)))
         (dispatch
           (lambda (msg)
             (case msg
               ((for-each) bullet-for-each)
               ((shoot!) shoot!)
               ((move!) move!)
               ((draw!) draw!)))))
    dispatch))

(define score 0)
(define (game-init name)
  (let* ((render (render-init name window-width window-height))
         (rocket (rocket-adt (render 'rocket-id)))
         (bullets (bullets-adt (render 'bullet-id)))
         (aliens (swarm-adt (render 'alien-id)))
         (loaded? #t)
         (shooting? #f)
         (shoot!
           (lambda ()
             (when loaded?
               (set! loaded? #f)
               (let* ((x (- (+ (rocket 'pos-x) (/ rocket-width 2))
                            (/ bullet-width 2)))
                      (y (- (rocket 'pos-y) bullet-height)))
                 ((bullets 'shoot!) x y)))))
         (start
           (lambda ()
             (let* ((rocket-time 0)
                    (bullet-time 0)
                    (reload-time 0)
                    (alien-time  0)
                    (game-loop-fun
                      (lambda (delta-t)
                        (when shooting?
                          (shoot!))
                        (set! reload-time (+ reload-time delta-t))
                        (when (> reload-time reload-timer)
                          (set! loaded? #t)
                          (set! reload-time 0))
                        (set! rocket-time (+ rocket-time delta-t))
                        (when (> rocket-time rocket-speed)
                          ((rocket 'move!))
                          (set! rocket-time 0))
                        (set! bullet-time (+ bullet-time delta-t))
                        (when (> bullet-time bullet-speed)
                          ((bullets 'move!))
                          (let* ((t-b (aliens 'y-bounds))
                                 (l-r (aliens 'x-bounds))
                                 (top (car t-b))
                                 (bottom (cdr t-b))
                                 (left (car l-r))
                                 (right (cdr l-r)))
                            ((bullets 'for-each)
                             (lambda (b)
                               (let ((x (b 'x))
                                     (y (b 'y)))
                                 (when (and (>= y bottom)
                                            (<= y top)
                                            (>= x left)
                                            (<= x right))
                                   (let ((shot ((aliens 'shot!) x y)))
                                     (when shot
                                       (set! score (+ score shot))
                                       (displayln score)
                                       ((b 'explode!)))))))))
                          (set! bullet-time 0))
                        (set! alien-time (+ alien-time delta-t))
                        (when (> alien-time alien-speed)
                          ((aliens 'move!))
                          (set! alien-time 0))
                        ((rocket  'draw!) render)
                        ((bullets 'draw!) render)
                        ((aliens  'draw!) render)))
                    (key-fun
                      (lambda (key)
                        (case key
                          ((up #\space) (set! shooting? #t))
                          ((left)   ((rocket 'direction!) 'left #t))
                          ((right)  ((rocket 'direction!) 'right #t))
                          ((escape) (exit)))))
                    (release-fun
                      (lambda (key)
                        (case key
                          ((up #\space) (set! shooting? #f))
                          ((left)  ((rocket 'direction!) 'left #f))
                          ((right) ((rocket 'direction!) 'right #f))))))
               ((render 'set-game-loop-fun!) game-loop-fun)
               ((render 'set-key-release-fun!) release-fun)
               ((render 'set-key-fun!) key-fun))))
         (dispatch
           (lambda (msg)
             (case msg
               ((start) (start))))))
    dispatch))

(define game (game-init "main"))
(game 'start)
