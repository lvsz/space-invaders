#lang racket

(require "window.rkt")
(provide player-adt
         player-width
	     player-height)

;; unit sized proved by window.rkt
(define player-width  (* 13 unit-width))
(define player-height (*  8 unit-height))

;;; abstract data type for the player's ship/cannon
;;; id and make-id are objects provided by window.rkt
;;; they are used to generate and save a unique identifier
;;; and graphical representation for every instantiation
(define (player-adt id (name 'player) (mp #f))
  (let*
    (;; player lives
     ;; game over when hit with 0 left
     (lives 3)
     (active #t)
     (alive?
       (lambda ()
         (positive? lives)))

     ;; default X and Y positions
     (x (- (if mp mp 1) 1/2 (/ player-width 2)))
     (y 9/10)

     ;; bounds of the player's hitbox
     ;; gets called for collision checking
     (x-bounds
       (lambda ()
         (values x (+ x player-width))))
     (y-bounds
       (lambda ()
         (values (+ y player-height) y)))

     ;; gets called when hit by a bullet
     ;; returns 2 values
     ;; first one if it hit (currently can't return false)
     ;; second one is true if the game ends, false otherwise
     (shot!
       (lambda (x y)
         (if active
           (begin
             (set! lives (- lives 1))
             (if (zero? lives)
               ; bullet hit and game over
               (values #t #t)
               ; bullet hit, lost a life, game continues
               (values #t #f)))
           (values #f #f))))

     ;; moves the ship within the window's limits
     ;; only takes the symbols 'left and 'right as arguments
     ;; others get ignored
     (move!
       (let ((difference unit-width))
         (lambda (direction (left-border 0) (right-border 1))
           (when active
             (cond
               ; ship isn't touching right border, and it's going right
               ((and (eq? direction 'right) (< (+ x player-width) right-border))
                (set! x (+ x difference)))
               ; ship isn't touching left border, and it's going left
               ((and (eq? direction 'left) (> x left-border))
                (set! x (- x difference))))))))

     ;; sends draw message with id and coordinates to window adt
     (draw!
       (lambda (window)
         (if (alive?)
           ((window 'draw!) id x y)
           (begin ((window 'remove!) id)
                  (set! active #f)))))

     (dispatch
       (lambda (msg)
         (case msg
           ((x) x)
           ((y) y)
           ((name)  name)
           ((lives) lives)
           ((x-bounds) (x-bounds))
           ((y-bounds) (y-bounds))
           ((alive?)   (alive?))
           ((shot!)      shot!)
           ((move!)      move!)
           ((draw!)      draw!)
           (else
             (raise-arguments-error
               'player-adt
               "invalid argument"
               "given" msg))))))
    dispatch))

