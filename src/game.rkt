#lang racket

(require "window.rkt"
         "ring.rkt")

(provide (all-defined-out))

(define window-width 224)
(define window-height 256)

(define unit-width  1/224)
(define unit-height 1/256)

(define player-width  (* 13 unit-width))
(define player-height (*  8 unit-height))
(define bullet-width  (*  1 unit-width))
(define bullet-height (*  7 unit-height))

(define player-speed 15)
(define bullet-speed 15)
(define alien-speed  1000)
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
           (let ((x-diff (* 2 unit-width)))
             (lambda (direction)
               (when alive?
                 (case direction
                   ((left)  (set! x (- x x-diff)))
                   ((right) (set! x (+ x x-diff)))
                   ((down)  (set! y (+ y alien-height))))))))
         (draw!
           (lambda (window)
             ((window 'draw!) id x y)
             ((window 'animate!) id)))
         (dispatch
           (lambda (msg)
             (case msg
               ((x) x)
               ((y) y)
               ((alive?) alive?)
               ((kill!)  kill!)
               ((move!)  move!)
               ((draw!)  draw!)
               (else
                 (raise-arguments-error 'alien-adt
                                        "invalid argument"
                                        "given" msg))))))
    dispatch))

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
                 (let* ((mid  (quotient (+ i j) 2))
                        (aln  (alien mid))
                        (a-x1 (aln 'x))
                        (a-x2 (+ a-x1 alien-width)))
                   (cond ((< b-x a-x1)
                          (bsearch i (- mid 1)))
                         ((> b-x a-x2)
                          (bsearch (+ mid 1) j))
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
           (lambda (window)
             (vector-map (lambda (a) ((a 'draw!) window)) aliens)))
         (dispatch
           (lambda (msg)
             (case msg
               ((y) y)
               ((alive?) alive?)
               ((shot!)  shot!)
               ((move!)  move!)
               ((draw!)  draw!)
               (else
                 (raise-arguments-error 'alien-row
                                        "invalid argument"
                                        "given" msg))))))
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
           (lambda (b-x b-y)
             (let loop ((i bottom))
               (let* ((a-row (vector-ref aliens i))
                      (a-y (a-row 'y)))
                 (cond ((and (>= b-y a-y)
                             (<= b-y (+ a-y alien-height)))
                        ((a-row 'shot!) b-x))
                       ((= i top) #f)
                       (else (loop (+ i 1))))))))
         (move!
           (lambda ()
             (vector-map (lambda (row) ((row 'move!))) aliens)))
         (draw!
           (lambda (window)
             (vector-map (lambda (row) ((row 'draw!) window)) aliens)))
         (dispatch
           (lambda (msg)
             (case msg
               ((x-bounds) (cons 0 1));(x-bounds))
               ((y-bounds) (cons 1 0));(y-bounds))
               ((shot!) shot!)
               ((move!) move!)
               ((draw!) draw!)
               (else
                 (raise-arguments-error 'swarm-adt
                                        "invalid argument"
                                        "given" msg))))))
    dispatch))

(define (player-adt make-id)
  (let* ((id (make-id))
         (x (- 1/2 (/ player-width 2)))
         (y 9/10)
         (direction 0)
         (set-x!
           (lambda (new-x)
             (set! x new-x)))
         (set-y!
           (lambda (new-y)
             (set! y new-y)))
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
             (set-x! (+ x (* direction unit-width)))))
         (draw!
           (lambda (window)
             ((window 'draw!) id x y)))
         (dispatch
           (lambda (msg)
             (case msg
               ((x) x)
               ((y) y)
               ((direction!) direction!)
               ((move!)      move!)
               ((draw!)      draw!)
               (else
                 (raise-arguments-error 'player-adt
                                        "invalid argument"
                                        "given" msg))))))
    dispatch))

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
           (lambda (window)
             ((window 'draw!) id x y)))
         (dispatch
           (lambda (msg)
             (case msg
               ((x) x)
               ((y) y)
               ((reset!)    reset!)
               ((exploded?) exploded?)
               ((explode!)  explode!)
               ((move!)     move!)
               ((draw!)     draw!)
               (else
                 (raise-arguments-error 'bullet-adt
                                        "invalid argument"
                                        "given" msg))))))
    dispatch))

(define (bullets-adt make-id)
  (let* ((bullets (build-ring bullet-limit (lambda (_) (bullet-adt make-id))))
         (bullet-for-each
           (lambda (proc)
             (ring-for-each
               (lambda (b)
                 (unless (b 'exploded?) (proc b)))
               bullets)))
         (shoot!
           (lambda (x y)
             (ring-next! bullets)
             (let ((bullet (ring-head bullets)))
               ((bullet 'reset!) x y))))
         (move!
           (lambda ()
             (ring-for-each (lambda (b) ((b 'move!))) bullets)))
         (draw!
           (lambda (window)
             (ring-for-each (lambda (b) ((b 'draw!) window)) bullets)))
         (dispatch
           (lambda (msg)
             (case msg
               ((for-each) bullet-for-each)
               ((shoot!)   shoot!)
               ((move!)    move!)
               ((draw!)    draw!)
               (else
                 (raise-arguments-error 'bullets-adt
                                        "invalid argument"
                                        "given" msg))))))
    dispatch))

(define score 0)
(define (game-init name)
  (let* ((window  (window-adt name window-width window-height))
         (player  (player-adt  (window 'player-id)))
         (bullets (bullets-adt (window 'bullet-id)))
         (aliens  (swarm-adt   (window 'alien-id)))
         (loaded? #t)
         (shooting? #f)
         (shoot!
           (lambda ()
             (when loaded?
               (set! loaded? #f)
               (let* ((x (- (+ (player 'x) (/ player-width 2))
                            (/ bullet-width 2)))
                      (y (- (player 'y) bullet-height)))
                 ((bullets 'shoot!) x y)))))
         (start
           (lambda ()
             (let* ((player-time 0)
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
                        (set! player-time (+ player-time delta-t))
                        (when (> player-time player-speed)
                          ((player 'move!))
                          (set! player-time 0))
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
                          (set! alien-time 0)
                          ((aliens  'draw!) window))
                        ((player  'draw!) window)
                        ((bullets 'draw!) window)))
                    (key-fun
                      (lambda (key)
                        (case key
                          ((up #\space) (set! shooting? #t))
                          ((left)   ((player 'direction!) 'left #t))
                          ((right)  ((player 'direction!) 'right #t))
                          ((escape) (exit)))))
                    (release-fun
                      (lambda (key)
                        (case key
                          ((up #\space) (set! shooting? #f))
                          ((left)  ((player 'direction!) 'left #f))
                          ((right) ((player 'direction!) 'right #f))))))
               ((window 'set-game-loop-fun!) game-loop-fun)
               ((window 'set-key-release-fun!) release-fun)
               ((window 'set-key-fun!) key-fun))))
         (dispatch
           (lambda (msg)
             (case msg
               ((start) (start))))))
    dispatch))

(define game (game-init "main"))
(game 'start)
