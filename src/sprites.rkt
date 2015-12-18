#lang racket

(require "Graphics.rkt")

(provide (all-defined-out))

(define (alien-R1)
  (make-tile-sequence
    (list (make-bitmap-tile
            "../gfx/1-1-R.png"
            "../gfx/1-1-mask.png")
          (make-bitmap-tile
            "../gfx/1-2-R.png"
            "../gfx/1-2-mask.png"))))

(define (alien-B1)
  (make-tile-sequence
    (list (make-bitmap-tile
            "../gfx/1-1-B.png"
            "../gfx/1-1-mask.png")
          (make-bitmap-tile
            "../gfx/1-2-B.png"
            "../gfx/1-2-mask.png"))))

(define (alien-B2)
  (make-tile-sequence
    (list (make-bitmap-tile
            "../gfx/2-1-B.png"
            "../gfx/2-1-mask.png")
          (make-bitmap-tile
            "../gfx/2-2-B.png"
            "../gfx/2-2-mask.png"))))

(define (alien-G2)
  (make-tile-sequence
    (list (make-bitmap-tile
            "../gfx/2-1-G.png"
            "../gfx/2-1-mask.png")
          (make-bitmap-tile
            "../gfx/2-2-G.png"
            "../gfx/2-2-mask.png"))))

(define (alien-G3)
  (make-tile-sequence
    (list (make-bitmap-tile
            "../gfx/3-1-G.png"
            "../gfx/3-1-mask.png")
          (make-bitmap-tile
            "../gfx/3-2-G.png"
            "../gfx/3-2-mask.png"))))

