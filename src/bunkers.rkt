#lang racket

(require "sprites.rkt")
(require "window.rkt")

(provide bunkers-adt)

(define bitmap-width  22)
(define bitmap-height 16)

(define bunker-width  (* bitmap-width  unit-width))
(define bunker-height (* bitmap-height unit-height))
(define margin  (* unit-width 32))
(define spacing (* unit-width 24))
(define bunker-y (* unit-width 192))
(define number-of-bunkers 4)

(define (x->bunker-x x offset)
  (/ (- x offset) unit-width))
(define (y->bunker-y y)
  (round (/ (- y bunker-y) unit-height)))

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

(define (bunker-adt make-id x y)
  (let*
    ((bitmap (vector-copy bunker-bitmap))
     (id (make-id bitmap bitmap-width bitmap-height))
     (updated? #f)
     (bit?
       (lambda (x y)
         (and (>= x 0)
              (>= y 0)
              (< x bitmap-width)
              (< y bitmap-height)
              (vector-ref bitmap (+ x (* bitmap-width y))))))
     (shot!
       (lambda (x direction)
         (let-values (((start end next y) (if (eq? direction 'up)
                                          (values (- bitmap-height 1) -1 -1 #f)
                                          (values 0 bitmap-height 1 #f))))
         (if (let loop ((i start))
               (cond ((= i end) #f)
                     ((bit? x i) (set! y i))
                     (else (loop (+ i next)))))
           (begin
             (vector-set! bitmap (+ x (* bitmap-width y)) #f)
             (when (bit? x (+ y next))
               (vector-set! bitmap (+ x (* bitmap-width (+ y next))) #f))
             (let loop ((i (- x 2)) (j (- y 2)))
               (when (and (bit? i j)
                          (zero? (random (+ 1 (abs (- i x)) (abs (- j y))))))
                 (vector-set! bitmap (+ i (* bitmap-width j)) #f))
               (cond ((= j (+ y 1))
                      (set! updated? #f))
                     ((= i (+ x 1))
                      (loop (- x 2) (+ j 1)))
                     (else
                       (loop (+ i 1) j)))))
           #f))))
     (draw!
       (lambda (window)
         (when (not updated?)
           ((window 'remove!) id)
           (set! id (make-id bitmap bitmap-width bitmap-height))
           (set! updated? #t)
           ((window 'draw!) id x y))))
     (dispatch
       (lambda (msg)
         (case msg
           ((shot!) shot!)
           ((draw!) draw!)))))
    dispatch))

(define (bunkers-adt make-id)
  (let*
    ((bunker-list
       (build-list number-of-bunkers
                   (lambda (i)
                     (bunker-adt make-id
                                 (+ margin (* i (+ bunker-width spacing)))
                                 bunker-y))))
     (shot!
       (lambda (x y direction)
         (if (or (<  y bunker-y)
                 (>= y (+ bunker-y bunker-height)))
           #f
           (let loop ((bunkers bunker-list) (offset margin))
             (cond ((null? bunkers) #f)
                   ((and (>= x offset)
                         (<  x (+ offset bunker-width)))
                    ;(displayln (cons (x->bunker-x x offset) (y->bunker-y y))))
                    (((car bunkers) 'shot!) (x->bunker-x x offset) direction))
                   (else (loop (cdr bunkers) (+ offset bunker-width spacing))))))))
     (draw!
       (lambda (window)
         (for-each (lambda (bunker) ((bunker 'draw!) window)) bunker-list)))
     (dispatch
       (lambda (msg)
         (case msg
           ((shot!) shot!)
           ((draw!) draw!)))))
    dispatch))

