;#lang racket

(require "render.rkt"
         (prefix-in ring: "ring.rkt"))

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
             (render 'draw! (list id x y))))
         (dispatch-alien
           (lambda (msg . opt)
             (case msg
               ((id) id)
               ((pos-x) x)
               ((pos-y) y)
               ((alive?) alive?)
               ((move!) (apply move! opt))
               ((draw!) (apply draw! opt))))))
    dispatch-alien))

(define (alien-row x y make-id)
  (let* ((aliens
           (build-vector
             aliens/row
             (lambda (i)
               (alien-adt (+ x (* 4/3 i alien-width)) y (make-id)))))
         (first-alien
           (lambda ()
             ((vector-ref aliens 0) 'pos-x)))
         (last-alien
           (lambda ()
             (+ alien-width ((vector-ref aliens 10) 'pos-x))))
         (direction 'right)
         (move!
         (lambda ()
           (cond
             ((and (= 1 (last-alien))
                   (eq? direction 'right))
              (set! direction 'left)
              (vector-map (lambda (a) (a 'move! 'down)) aliens))
             ((and (= 0 (first-alien))
                   (eq? direction 'left))
              (set! direction 'right)
              (vector-map (lambda (a) (a 'move! 'down)) aliens))
             (else
              (vector-map (lambda (a) (a 'move! direction)) aliens)))))
         (draw!
           (lambda (render)
             (vector-map (lambda (alien) (alien 'draw! render)) aliens)))
         (dispatch
           (lambda (msg . opt)
             (case msg
               ((move!) (move!))
               ((draw!) (apply draw! opt))))))
    dispatch))

(define (swarm-adt make-id)
  (let* ((x 0)
         (y 0)
         (aliens (vector (alien-row x y make-id)))
         (move!
           (lambda ()
             (vector-map (lambda (row) (row 'move!)) aliens)))
         (draw!
           (lambda (render)
             (vector-map (lambda (row) (row 'draw! render)) aliens)))
         (dispatch
           (lambda (msg . opt)
             (case msg
               ((move!) (move!))
               ((draw!) (apply draw! opt))))))
    dispatch))

(define (rocket-adt (id #f))
  (let* ((pos-x (- 1/2 (/ rocket-width 2)))
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
             (render 'draw! (list id pos-x pos-y))))
         (dispatch-rocket
           (lambda (msg . opt)
             (case msg
               ((id) id)
               ((pos-x) pos-x)
               ((pos-y) pos-y)
               ((direction!) (apply direction! opt))
               ((move!) (move!))
               ((draw!) (apply draw! opt))
               (else (error "unknown rocket-adt command:" msg))))))
    dispatch-rocket))

(define (bullet-adt x y (id #f))
  (let* ((exploded? #f)
         (explode!
           (lambda ()
             (set! exploded? #t)))
         (move!
           (lambda ()
             (set! y (- y unit-height))))
         (draw!
           (lambda (render)
             (render 'draw! (list id x y))))
         (dispatch-bullet
           (lambda (msg . opt)
             (case msg
               ((id) id)
               ((pos-x) x)
               ((pos-y) y)
               ((explode!) (explode!))
               ((move!) (unless exploded? (move!)))
               ((draw!) (apply draw! opt))))))
    dispatch-bullet))

(define (bullets-adt)
  (let* ((bullets (ring:new bullet-limit))
         (shoot!
           (lambda (x y (id #f))
             (ring:add! bullets (bullet-adt x y id))))
         (move!
           (lambda ()
             (ring:for-each (lambda (b) (b 'move!)) bullets)))
         (draw!
           (lambda (render)
             (ring:for-each (lambda (b) (b 'draw! render)) bullets)))
         (dispatch
           (lambda (msg . opt)
             (case msg
               ((shoot!) (apply shoot! opt))
               ((move!) (move!))
               ((draw!) (apply draw! opt))))))
    dispatch))

(define (game-init)
  (let* ((render (render-init "main" window-width window-height))
         (rocket (rocket-adt (render 'rocket-id)))
         (bullets (bullets-adt))
         (aliens (swarm-adt (lambda () (render 'alien-id))))
         (loaded? #t)
         (shooting? #f)
         (shoot!
           (lambda ()
             (when loaded?
               (set! loaded? #f)
               (let* ((x (- (+ (rocket 'pos-x) (/ rocket-width 2))
                            (/ bullet-width 2)))
                      (y (- (rocket 'pos-y) rocket-height)))
                 (bullets 'shoot! x y (render 'bullet-id))))))
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
                          (rocket 'move!)
                          (set! rocket-time 0))
                        (set! bullet-time (+ bullet-time delta-t))
                        (when (> bullet-time bullet-speed)
                          (bullets 'move!)
                          (set! bullet-time 0))
                        (set! alien-time (+ alien-time delta-t))
                        (when (> alien-time alien-speed)
                          (aliens 'move!)
                          (set! alien-time 0))
                        (rocket  'draw! render)
                        (bullets 'draw! render)
                        (aliens  'draw! render)))
                    (key-fun
                      (lambda (key)
                        (case key
                          ((up #\space) (set! shooting? #t))
                          ((left)   (rocket 'direction! 'left #t))
                          ((right)  (rocket 'direction! 'right #t))
                          ((escape) (exit)))))
                    (release-fun
                      (lambda (key)
                        (case key
                          ((up #\space) (set! shooting? #f))
                          ((left)  (rocket 'direction! 'left #f))
                          ((right) (rocket 'direction! 'right #f))))))
               (render 'set-game-loop-fun! game-loop-fun)
               (render 'set-key-release-fun! release-fun)
               (render 'set-key-fun! key-fun))))
         (dispatch
           (lambda (msg)
             (case msg
               ((start) (start))
               (else (eval msg))))))
    dispatch))


(define game (game-init)) (game 'start)
