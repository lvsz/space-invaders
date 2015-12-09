#lang racket

(require "render.rkt")
(require (prefix-in ring: "ring.rkt"))

(define window-width 1000)
(define window-height 600)

(define px-unit-width 4)
(define px-unit-height 4)

(define unit-width  (/ (/ window-width  px-unit-width)))
(define unit-height (/ (/ window-height px-unit-height)))


(define rocket-width  (* 15/2 unit-width))
(define rocket-height (*  9/2 unit-height))
(define bullet-width  unit-width)
(define bullet-height (* 2 unit-height))

(define rocket-speed 100)
(define bullet-speed 100)
(define bullet-limit  20)

(define (rocket-adt)
  (letrec ((pos-x (- 1/2 (/ rocket-width 2)))
           (pos-y 9/10)
           (direction 0)
           (set-x!
             (lambda (x)
               (set! pos-x x)))
           (set-y!
             (lambda (y)
               (set! pos-y y)))
           (direction!
             (lambda (key)
               (case key
                 ((left)  (set! direction -1))
                 ((right) (set! direction  1))
                 (else    (set! direction  0)))))
           (move!
             (lambda ()
               (set-x! (+ pos-x (* direction rocket-width)))))
           (draw!
             (lambda (render)
               (render 'draw-rocket! dispatch)))
           (dispatch
             (lambda (msg (opt #f))
               (case msg
                 ((pos-x) pos-x)
                 ((pos-y) pos-y)
                 ((direction!) (direction! opt))
                 ((move!) (move!))
                 ((draw!) (draw! opt))
                 (else (error "unknown rocket-adt command:" msg))))))
    (lambda args
      (if (eq? (car args) 'draw!)
        (draw! (cadr args))
        (apply dispatch args)))))

(define (bullet-adt x y)
  (let* ((exploded? #f)
           (explode!
             (lambda ()
               (set! exploded? #t)))
           (move!
             (lambda ()
               (set! y (- y rocket-height))))
           (dispatch-bullet
             (lambda (msg (opt #f))
               (case msg
                 ((pos-x) x)
                 ((pos-y) y)
                 ((explode!) (explode!))
                 ((move!) (unless exploded? (move!))))))
           (draw!
             (lambda (render)
               (render 'draw-bullet! dispatch-bullet))))
   ; dispatch-bullet))
    (lambda args
      (if (eq? (car args) 'draw!)
        (draw! (cadr args))
        (apply dispatch-bullet args)))))

(define (bullets-adt)
  (let* ((bullets (ring:new bullet-limit))
           (shoot!
             (lambda (x y)
               (let ((new-bullet (bullet-adt x y)))
                 (ring:add! bullets new-bullet)
                 new-bullet)))
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
                 ((move!) (move!))))))
    (lambda args
      (if (eq? (car args) 'draw!)
        (draw! (cadr args))
        (apply dispatch args)))))

(define (game-init)
  (let* ((rocket (rocket-adt))
         (bullets (bullets-adt))
         (render (render-init "main" window-width window-height bullet-limit))
         (shoot!
           (lambda ()
             (let* ((x (- (+ (rocket 'pos-x) (/ rocket-width 2))
                          (/ bullet-width 2)))
                    (y (- (rocket 'pos-y) rocket-height))
                    (new-bullet (bullets 'shoot! x y)))
               (render 'add-bullet! new-bullet))))
         (start
           (lambda ()
             (let* ((rocket-time 0)
                    (bullet-time 0)
                    (game-loop-fun
                      (lambda (delta-t)
                        (set! rocket-time (+ rocket-time delta-t))
                        (when (> rocket-time rocket-speed)
                          (rocket 'move!)
                          (set! rocket-time 0))
                        (set! bullet-time (+ bullet-time delta-t))
                        (when (> bullet-time bullet-speed)
                          (bullets 'move!)
                          (set! bullet-time 0))
                        (rocket  'draw! render)
                        (bullets 'draw! render)))
                    (key-fun
                      (lambda (key)
                        (case key
                          ((up #\space) (shoot!))
                          (else (rocket 'direction! key))))))
               (render 'set-game-loop-fun! game-loop-fun)
               (render 'set-key-fun! key-fun))))
         (dispatch
           (lambda (msg)
             (case msg
               ((start) (start))))))
    dispatch))

(define game (game-init))

(game 'start)
