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
         (move!
           (lambda (direction)
             (case direction
               ((left)  (set! x (- x unit-width)))
               ((right) (set! x (+ x unit-width)))
               ((down)  (set! y (+ y alien-height))))))
         (draw!
           (lambda (render)
             ((render 'draw!) id x y)))
         (dispatch-alien
           (lambda (msg . opt)
             (case msg
               ((id) id)
               ((pos-x) x)
               ((pos-y) y)
               ((alive?) alive?)
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
                 ((alien idx) 'pos-x))))
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
               (+ alien-width ((alien idx) 'pos-x)))))
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
               ((alive?) alive?)
               ((move!) move!)
               ((draw!) draw!)))))
    dispatch))

(define (swarm-adt make-id)
  (let* ((x 0)
         (y 0)
         (aliens (vector (alien-row 3 x y make-id)
                         (alien-row 2 x (* 2 alien-height) make-id)
                         (alien-row 1 x (* 4 alien-height) make-id)))
         (move!
           (lambda ()
             (vector-map (lambda (row) ((row 'move!))) aliens)))
         (draw!
           (lambda (render)
             (vector-map (lambda (row) ((row 'draw!) render)) aliens)))
         (dispatch
           (lambda (msg)
             (case msg
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
               ((pos-x) x)
               ((pos-y) y)
               ((reset!) reset!)
               ((explode!) explode!)
               ((move!) move!)
               ((draw!) draw!)))))
    dispatch-bullet))

(define (bullets-adt make-id)
  (let* ((bullets (build-ring bullet-limit (lambda (_) (bullet-adt make-id))))
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
               ((shoot!) shoot!)
               ((move!) move!)
               ((draw!) draw!)))))
    dispatch))

(define (game-init)
  (let* ((render (render-init "main" window-width window-height))
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

(define game (game-init))
(game 'start)
