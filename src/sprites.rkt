#lang racket

(require "Graphics.rkt")

(provide text-tile
         player-tile
         bullet-tile
         invader-a
         invader-b
         invader-c
         explosion-tile
         bunker-tile)

(define · #f)
(define █ #t)
(define max-text-size 120)
(define default-font-size 14)

(struct sprite (width height vector))

(define (sprite-ref s x y)
  (vector-ref (sprite-vector s) (+ x (* (sprite-width s) y))))

(define (remove-pixel! s x y)
  (vector-set! (sprite-vector s) (+ x (* (sprite-width s) y)) #f))


(define player
  (sprite 13 8
          (vector
            · · · · · · █ · · · · · ·
            · · · · · █ █ █ · · · · ·
            · · · · · █ █ █ · · · · ·
            · █ █ █ █ █ █ █ █ █ █ █ ·
            █ █ █ █ █ █ █ █ █ █ █ █ █
            █ █ █ █ █ █ █ █ █ █ █ █ █
            █ █ █ █ █ █ █ █ █ █ █ █ █
            █ █ █ █ █ █ █ █ █ █ █ █ █)))

(define bullet
  (sprite 1 4
          (vector
            █
            █
            █
            █)))

(define invader-a1
  (sprite 12 8
          (vector
            · · · · █ █ █ █ · · · ·
            · █ █ █ █ █ █ █ █ █ █ ·
            █ █ █ █ █ █ █ █ █ █ █ █
            █ █ █ · · █ █ · · █ █ █
            █ █ █ █ █ █ █ █ █ █ █ █
            · · █ █ █ · · █ █ █ · ·
            · █ █ · · █ █ · · █ █ ·
            · · █ █ · · · · █ █ · ·)))

(define invader-a2
  (sprite 12 8
          (vector
            · · · · █ █ █ █ · · · ·
            · █ █ █ █ █ █ █ █ █ █ ·
            █ █ █ █ █ █ █ █ █ █ █ █
            █ █ █ · · █ █ · · █ █ █
            █ █ █ █ █ █ █ █ █ █ █ █
            · · · █ █ · · █ █ · · ·
            · · █ █ · █ █ · █ █ · ·
            █ █ · · · · · · · · █ █)))

(define invader-b1
  (sprite 11 8
          (vector
            · · █ · · · · · █ · ·
            · · · █ · · · █ · · ·
            · · █ █ █ █ █ █ █ · ·
            · █ █ · █ █ █ · █ █ ·
            █ █ █ █ █ █ █ █ █ █ █
            █ · █ █ █ █ █ █ █ · █
            █ · █ · · · · · █ · █
            · · · █ █ · █ █ · · ·)))

(define invader-b2
  (sprite 11 8
          (vector
            · · █ · · · · · █ · ·
            █ · · █ · · · █ · · █
            █ · █ █ █ █ █ █ █ · █
            █ █ █ · █ █ █ · █ █ █
            █ █ █ █ █ █ █ █ █ █ █
            · █ █ █ █ █ █ █ █ █ ·
            · · █ · · · · · █ · ·
            · █ · · · · · · · █ ·)))

(define invader-c1
  (sprite 8 8
          (vector
            · · · █ █ · · ·
            · · █ █ █ █ · ·
            · █ █ █ █ █ █ ·
            █ █ · █ █ · █ █
            █ █ █ █ █ █ █ █
            · █ · █ █ · █ ·
            █ · · · · · · █
            · █ · · · · █ ·)))

(define invader-c2
  (sprite 8 8
          (vector
            · · · █ █ · · ·
            · · █ █ █ █ · ·
            · █ █ █ █ █ █ ·
            █ █ · █ █ · █ █
            █ █ █ █ █ █ █ █
            · · █ · · █ · ·
            · █ · █ █ · █ ·
            █ · █ · · █ · █)))

(define explosion
  (sprite 13 8
          (vector
            · · · · █ · · · █ · · · ·
            · █ · · · █ · █ · · · █ ·
            · · █ · · · · · · · █ · ·
            · · · █ · · · · · █ · · ·
            █ █ · · · · · · · · · █ █
            · · · █ · · · · · █ · · ·
            · · █ · · █ · █ · · █ · ·
            · █ · · █ · · · █ · · █ ·)))


(define (sprite->tile s color)
  (let* ((width (sprite-width s))
         (height (sprite-height s))
         (tile (make-tile width height #f #f)))
    (let loop ((x 0) (y 0))
      (cond ((= y height) tile)
            ((= x width) (loop 0 (+ y 1)))
            (else (when (sprite-ref s x y)
                    ((tile 'draw-point) x y color))
                  (loop (+ x 1) y))))))

(define (text-tile text (color "white"))
  (let ((tile (make-tile max-text-size default-font-size #f #f)))
    ((tile 'draw-text) text default-font-size 0 0 color)
    tile))

(define (player-tile)
  (sprite->tile player "cyan"))

(define (bullet-tile)
  (sprite->tile bullet "yellow"))

(define (invader-a color)
  (make-tile-sequence
    (list (sprite->tile invader-a1 color)
          (sprite->tile invader-a2 color))))

(define (invader-b color)
  (make-tile-sequence
    (list (sprite->tile invader-b1 color)
          (sprite->tile invader-b2 color))))

(define (invader-c color)
  (make-tile-sequence
    (list (sprite->tile invader-c1 color)
          (sprite->tile invader-c2 color))))

(define (explosion-tile color)
  (sprite->tile explosion color))

(define (bunker-tile bitmap width height)
  (sprite->tile (sprite width height bitmap) "red"))

