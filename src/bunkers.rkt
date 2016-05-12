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
(define y (* unit-width 192))
(define number-of-bunkers 4)

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
         (vector-ref bitmap (+ x (* bitmap-width y)))))
     (shot!
       (lambda (x y)
         (when (bit? x y)
           (vector-set! bitmap (+ x (* bitmap-width y)) #f)
           (set! updated? #f))))
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
    ((bunkers
       (build-list number-of-bunkers
                   (lambda (i)
                     (bunker-adt make-id
                                 (+ margin (* i (+ bunker-width spacing)))
                                 y))))
     (draw!
       (lambda (window)
         (for-each (lambda (bunker) ((bunker 'draw!) window)) bunkers)))
     (dispatch
       (lambda (msg)
         (case msg
           ((draw!) draw!)))))
    dispatch))

