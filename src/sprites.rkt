#lang racket

(require "Graphics.rkt")

(provide (all-defined-out))

;;; seperate file remove filepath clutter in the other files

(define (invader-death)
  (make-bitmap-tile
    "../gfx/invaders/death.png"
    "../gfx/invaders/death-mask.png"))

(define (invader-R1)
  (make-tile-sequence
    (list (make-bitmap-tile
            "../gfx/invaders/1-1-R.png"
            "../gfx/invaders/1-1-mask.png")
          (make-bitmap-tile
            "../gfx/invaders/1-2-R.png"
            "../gfx/invaders/1-2-mask.png"))))

(define (invader-B1)
  (make-tile-sequence
    (list (make-bitmap-tile
            "../gfx/invaders/1-1-B.png"
            "../gfx/invaders/1-1-mask.png")
          (make-bitmap-tile
            "../gfx/invaders/1-2-B.png"
            "../gfx/invaders/1-2-mask.png"))))

(define (invader-B2)
  (make-tile-sequence
    (list (make-bitmap-tile
            "../gfx/invaders/2-1-B.png"
            "../gfx/invaders/2-1-mask.png")
          (make-bitmap-tile
            "../gfx/invaders/2-2-B.png"
            "../gfx/invaders/2-2-mask.png"))))

(define (invader-G2)
  (make-tile-sequence
    (list (make-bitmap-tile
            "../gfx/invaders/2-1-G.png"
            "../gfx/invaders/2-1-mask.png")
          (make-bitmap-tile
            "../gfx/invaders/2-2-G.png"
            "../gfx/invaders/2-2-mask.png"))))

(define (invader-G3)
  (make-tile-sequence
    (list (make-bitmap-tile
            "../gfx/invaders/3-1-G.png"
            "../gfx/invaders/3-1-mask.png")
          (make-bitmap-tile
            "../gfx/invaders/3-2-G.png"
            "../gfx/invaders/3-2-mask.png"))))

