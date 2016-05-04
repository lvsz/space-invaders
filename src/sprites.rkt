#lang racket

(require "Graphics.rkt")

(provide player-tile
         bullet-tile
         invader-a
         invader-b
         invader-c
         explosion-tile
         exit-tile
         pointer-tile
         start-tile)

(define - #f)
(define X #t)
(define point? car)

(define player
    (list - - - - - - X - - - - - -
          - - - - - X X X - - - - -
          - - - - - X X X - - - - -
          - X X X X X X X X X X X -
          X X X X X X X X X X X X X
          X X X X X X X X X X X X X
          X X X X X X X X X X X X X
          X X X X X X X X X X X X X))

(define bullet
  (list X
        X
        X
        X
        X
        X
        X))

(define invader-a1
  (list - - - - X X X X - - - -
        - X X X X X X X X X X -
        X X X X X X X X X X X X
        X X X - - X X - - X X X
        X X X X X X X X X X X X
        - - X X X - - X X X - -
        - X X - - X X - - X X -
        - - X X - - - - X X - -))

(define invader-a2
  (list - - - - X X X X - - - -
        - X X X X X X X X X X -
        X X X X X X X X X X X X
        X X X - - X X - - X X X
        X X X X X X X X X X X X
        - - - X X - - X X - - -
        - - X X - X X - X X - -
        X X - - - - - - - - X X))

(define invader-b1
  (list - - X - - - - - X - -
        - - - X - - - X - - -
        - - X X X X X X X - -
        - X X - X X X - X X -
        X X X X X X X X X X X
        X - X X X X X X X - X
        X - X - - - - - X - X
        - - - X X - X X - - -))

(define invader-b2
  (list - - X - - - - - X - -
        X - - X - - - X - - X
        X - X X X X X X X - X
        X X X - X X X - X X X
        X X X X X X X X X X X
        - X X X X X X X X X -
        - - X - - - - - X - -
        - X - - - - - - - X -))

(define invader-c1
  (list - - - X X - - -
        - - X X X X - -
        - X X X X X X -
        X X - X X - X X
        X X X X X X X X
        - X - X X - X -
        X - - - - - - X
        - X - - - - X -))

(define invader-c2
  (list - - - X X - - -
        - - X X X X - -
        - X X X X X X -
        X X - X X - X X
        X X X X X X X X
        - - X - - X - -
        - X - X X - X -
        X - X - - X - X))


(define explosion
  (list - - - - X - - - X - - - -
        - X - - - X - X - - - X -
        - - X - - - - - - - X - -
        - - - X - - - - - X - - -
        X X - - - - - - - - - X X
        - - - X - - - - - X - - -
        - - X - - X - X - - X - -
        - X - - X - - - X - - X -))

(define (list->tile lst w h color)
  (let ((tile (make-tile w h #f #f)))
    (do ((i 0 (+ i 1)) 
         (bmp lst (cdr bmp)))
      ((null? bmp) tile)
     ;; debug code
     ;; prints ascii representation to stdout
     ;   (if (point? bmp)
     ;     (display 'X)
     ;     (display '-)) 
     ;   (when (= (modulo (+ i 1) w) 0)
     ;     (newline))
      (when (point? bmp)
        (let-values (((y x) (quotient/remainder i w)))
          ((tile 'draw-point) x y color))))))

(define (player-tile)
  (list->tile player 13 8 "cyan"))

(define (bullet-tile)
  (list->tile bullet 1 7 "yellow"))

(define (invader-a color)
  (make-tile-sequence
    (list (list->tile invader-a1 12 8 color)
          (list->tile invader-a2 12 8 color))))

(define (invader-b color)
  (make-tile-sequence
    (list (list->tile invader-b1 11 8 color)
          (list->tile invader-b2 11 8 color))))

(define (invader-c color)
  (make-tile-sequence
    (list (list->tile invader-c1 8 8 color)
          (list->tile invader-c2 8 8 color))))

(define (explosion-tile)
  (list->tile explosion 13 8 "white"))

(define (exit-tile)
  (let ((tile (make-tile 52 12 #f #f)))
    ((tile 'draw-text) "EXIT" 12 0 0 "white")
    tile))

  (define (start-tile)
    (let ((tile (make-tile 52 12 #f #f)))
      ((tile 'draw-text) "START" 12 0 0 "white")
      tile))

  (define (pointer-tile)
    (let ((tile (make-tile 7 12 #f #f)))
      ((tile 'draw-text) ">" 12 0 0 "white")
      tile))

