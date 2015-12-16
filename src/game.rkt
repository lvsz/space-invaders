#lang racket

(require "window.rkt"
         "ring.rkt"
         "aliens.rkt")

(provide (all-defined-out))

(define player-width  (* 13 unit-width))
(define player-height (*  8 unit-height))
(define bullet-width  (*  1 unit-width))
(define bullet-height (*  7 unit-height))

(define player-speed 15)
(define bullet-speed 15)
(define bullet-limit 10)
(define reload-timer 800)


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
                 (raise-arguments-error
                   'player-adt
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
               (set! y (- y bullet-height)))))
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
                 (raise-arguments-error
                   'bullet-adt
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
                 (raise-arguments-error
                   'bullets-adt
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
                          ((player  'draw!) window)
                        (set! bullet-time (+ bullet-time delta-t))
                        (when (> bullet-time bullet-speed)
                          (let-values (((top bottom) (aliens 'y-bounds))
                                       ((left right) (aliens 'x-bounds)))
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
                          ((bullets 'move!))
                          ((bullets 'draw!) window)
                        (set! alien-time (+ alien-time delta-t))
                        (when (> alien-time (aliens 'speed))
                          ((aliens 'move!))
                          (set! alien-time 0)
                          ((aliens 'draw!) window))))
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
