#lang racket

(require "sprites.rkt")
(require "window.rkt")

(provide bunkers-adt)

;; dimensions of the bitmap below
(define bitmap-width  22)
(define bitmap-height 16)

;; dimensions that work for the window
(define bunker-width  (* bitmap-width  unit-width))
(define bunker-height (* bitmap-height unit-height))

;; spacing between edge of screen and first bunker
(define margin  (* unit-width 32))
;; spacing between bunkers
(define spacing (* unit-width 24))
;; height on screen of bunkers
(define bunker-y (* unit-width 192))
;; number of bunkers
(define number-of-bunkers 4)

;; converts an x coordinate of the window
;; to a corresponding location in the bitmap
(define (x->bunker-x x offset)
  (/ (- x offset) unit-width))

;; the basic bunker bitmap
;; gets modified during game-play
(define bunker-bitmap 
  (let ((· #f)
        (█ #t))
    (vector
      · · · · █ █ █ █ █ █ █ █ █ █ █ █ █ █ · · · ·
      · · · █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ · · ·
      · · █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ · ·
      · █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ ·
      █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
      █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
      █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
      █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
      █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
      █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
      █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
      █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █ █
      █ █ █ █ █ █ █ · · · · · · · · █ █ █ █ █ █ █
      █ █ █ █ █ █ · · · · · · · · · · █ █ █ █ █ █
      █ █ █ █ █ · · · · · · · · · · · · █ █ █ █ █
      █ █ █ █ █ · · · · · · · · · · · · █ █ █ █ █)))


;;; single bunker ADT
;;; for local use only
(define (bunker-adt make-id x y)
  (let*
    (;; several bunkers needed, so make copies
     (bitmap (vector-copy bunker-bitmap))
     (id (make-id bitmap bitmap-width bitmap-height))
     ;; when #f, needs updating
     (updated #f)

     ;; check if a certain bit exists and is #t
     (bit?
       (lambda (x y)
         (and (>= x 0)
              (>= y 0)
              (< x bitmap-width)
              (< y bitmap-height)
              (vector-ref bitmap (+ x (* bitmap-width y))))))

     ;; colission detection
     (shot!
       (lambda (x direction)
         (let-values (((start end next y)
                       (if (eq? direction 'up)
                         (values (- bitmap-height 1) -1 -1 #f)
                         (values 0 bitmap-height 1 #f))))
           ;; first see which direction the bullet travels
           ;; then set the y coordinate of the right bit to damage
           ;; returns #f is bullet travels through
           (if (let loop ((i start))
                 (cond ((= i end) #f)
                       ((bit? x i) (set! y i))
                       (else (loop (+ i next)))))
             (begin
               (vector-set! bitmap (+ x (* bitmap-width y)) #f)
               (when (bit? x (+ y next))
                 (vector-set! bitmap (+ x (* bitmap-width (+ y next))) #f))
               (let loop ((i (- x 3)) (j (- y 3)))
                 ;; some randomness for collateral damage
                 (when (and (bit? i j)
                            (zero? (random (+ 1 (abs (- i x)) (abs (- j y))))))
                   (vector-set! bitmap (+ i (* bitmap-width j)) #f))
                 (cond ((= j (+ y 2))
                        (set! updated #f))
                       ((= i (+ x 2))
                        (loop (- x 3) (+ j 1)))
                       (else
                         (loop (+ i 1) j)))))
             #f))))

     ;; draws the bunker only if it needs updating
     (draw!
       (lambda (window)
         (when (not updated)
           ;; has to generate a new id
           ((window 'remove!) id)
           (set! id (make-id bitmap bitmap-width bitmap-height))
           (set! updated #t)
           ((window 'draw!) id x y))))

     (dispatch
       (lambda (msg)
         (case msg
           ((shot!) shot!)
           ((draw!) draw!)))))
    dispatch))


;;; ADT for all bunkers, gets provided to main.rkt
(define (bunkers-adt make-id)
  (let*
    (;; build the bunkers
     (bunker-list
       (build-list number-of-bunkers
                   (lambda (i)
                     (bunker-adt make-id
                                 (+ margin (* i (+ bunker-width spacing)))
                                 bunker-y))))

     ;; gets coordinates and sees if there might be a hit
     (shot!
       (lambda (x y direction)
         ;; most impartantly, the bullet can't be higher or lower than the bunkers
         (if (or (<  y bunker-y)
                 (>= y (+ bunker-y bunker-height)))
           #f
           (let loop ((bunkers bunker-list) (offset margin))
             (cond
               ; no bunkers hit
               ((null? bunkers) #f)
               ; found a target, might still return #f if too damaged
               ((and (>= x offset)
                     (<  x (+ offset bunker-width)))
                (((car bunkers) 'shot!) (x->bunker-x x offset) direction))
               ; else continue looking
               (else (loop (cdr bunkers) (+ offset bunker-width spacing))))))))

     ;; draws all available bunkers
     (draw!
       (lambda (window)
         (for-each (lambda (bunker) ((bunker 'draw!) window)) bunker-list)))

     (dispatch
       (lambda (msg)
         (case msg
           ((shot!) shot!)
           ((draw!) draw!)))))
    dispatch))

