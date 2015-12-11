;#lang racket

(require "render.rkt"
         (prefix-in ring: "ring.rkt"))

;(provide game-init)

(define window-width 224)
(define window-height 256)

(define px-unit-width 1)
(define px-unit-height 1)

(define unit-width  (/ px-unit-width  window-width))
(define unit-height (/ px-unit-height window-height))

(define rocket-width  (* 13 unit-width))
(define rocket-height (*  8 unit-height))
(define bullet-width  (*  1 unit-width))
(define bullet-height (*  7 unit-height))

(define rocket-speed 15)
(define bullet-speed 15)
(define bullet-limit 20)
(define reload-timer 1000)

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
           (let ((left  #f)
                 (right #f))
             (lambda (key status)
               (case key
                 ((left)  (set! left  status))
                 ((right) (set! right status)))
               (set! direction (+ (if left -1 0)
                                  (if right 1 0))))))
         (move!
           (lambda ()
             (set-x! (+ pos-x (* direction unit-width)))))
         (dispatch-rocket
           (lambda (msg (opt #f))
             (case msg
               ((id) id)
               ((pos-x) pos-x)
               ((pos-y) pos-y)
               ((direction!) (apply direction! opt))
               ((move!) (move!))
               (else (error "unknown rocket-adt command:" msg)))))
         (draw!
           (lambda (render)
             (render 'draw! (list id pos-x pos-y)))))
    (lambda args
      (if (eq? (car args) 'draw!)
        (apply draw! (cdr args))
        (apply dispatch-rocket args)))))

(define (bullet-adt x y (id #f))
  (let* ((exploded? #f)
         (explode!
           (lambda ()
             (set! exploded? #t)))
         (move!
           (lambda ()
             (set! y (- y unit-height))))
         (dispatch-bullet
           (lambda (msg (opt #f))
             (case msg
               ((id) id)
               ((pos-x) x)
               ((pos-y) y)
               ((explode!) (explode!))
               ((move!) (unless exploded? (move!))))))
         (draw!
           (lambda (render)
             (render 'draw! (list id x y)))))
    (lambda args
      (if (eq? (car args) 'draw!)
        (apply draw! (cdr args))
        (apply dispatch-bullet args)))))

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
               ((move!) (move!))))))
    (lambda args
      (if (eq? (car args) 'draw!)
        (apply draw! (cdr args))
        (apply dispatch args)))))

(define (game-init)
  (let* ((render (render-init "main" window-width window-height))
         (rocket (rocket-adt (render 'rocket-id)))
         (bullets (bullets-adt))
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
                        (rocket  'draw! render)
                        (bullets 'draw! render)))
                    (key-fun
                      (lambda (key)
                        (case key
                          ((up #\space) (set! shooting? #t))
                          ((left)   (rocket 'direction! '(left #t)))
                          ((right)  (rocket 'direction! '(right #t)))
                          ((escape) (exit)))))
                    (release-fun
                      (lambda (key)
                        (case key
                          ((up #\space) (set! shooting? #f))
                          ((left)  (rocket 'direction! '(left #f)))
                          ((right) (rocket 'direction! '(right #f)))))))
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
