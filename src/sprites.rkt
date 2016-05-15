#lang racket

(require "Graphics.rkt")

(provide text-tile
         score-tile
         player-tile
         bullet-tile
         invader-a
         invader-b
         invader-c
         explosion-tile
         bunker-tile)

;; sprites are basically bitmaps
;; #t means draw this as a pixel, #f means don't
;; these characters were chosen for their visual qualities,
;; not because they're easy to type
(define █ #t)
(define · #f)

;; struct for storing sprites
;; because width and height are given, the vector can be 1 dimensional
(struct sprite (width height vector))

;; uses given coordinates and a sprite's width to reference the right cell
(define (sprite-ref s x y)
  (vector-ref (sprite-vector s) (+ x (* (sprite-width s) y))))

;; when "drawing" sprites, it's recommended to use different characters
;; and replace them with a regular expression afterwards
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

;; each invader model gets 2 sprites for animation purposes
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


;; function that converts a sprite with a given color
;; to a tile usuable in Graphics.rkt
(define (sprite->tile s color)
  (let* ((width (sprite-width s))
         (height (sprite-height s))
         (tile (make-tile width height #f #f)))
    (let loop ((x 0) (y 0))
      (cond
        ; loop is finished
        ((= y height) tile)
        ; row is finished, so move to next
        ((= x width) (loop 0 (+ y 1)))
        ; else draw pixel if needed and continue
        (else (when (sprite-ref s x y)
                ((tile 'draw-point) x y color))
              (loop (+ x 1) y))))))

;; defaults for drawing text
(define max-text-size 120)
(define default-font-size 14)

;; generates a tile from a given string
;; if no color is given, white is used
(define (text-tile text (color "white"))
  (let ((tile (make-tile max-text-size default-font-size #f #f)))
    ((tile 'draw-text) text default-font-size 0 0 color)
    tile))

;; below are the tiles used in-game
(define (score-tile text)
  (let ((tile (make-tile max-text-size 12 #f #f)))
    ((tile 'draw-text) text 12 0 0 "white")
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

;; bunker is a special case, as the bitmap is changed each time it's shot
(define (bunker-tile bitmap width height)
  (sprite->tile (sprite width height bitmap) "red"))

