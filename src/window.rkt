#lang racket

(require "Graphics.rkt"
         "sprites.rkt")

(provide window-width
         window-height
         unit-width
         unit-height
         window-adt)

;; based on resolution of the original game
(define window-width 224)
(define window-height 256)

(define unit-width  1/224)
(define unit-height 1/256)

(define (window-adt name width height)
  (let* ((window (make-window width height name))
         (player-layer (window 'make-layer))
         (bullet-layer (window 'make-layer))
         (alien-layer  (window 'make-layer))

         (player-tile
           (lambda ()
             (let ((bmp (make-bitmap-tile "../gfx/player.png"
                                          "../gfx/player-mask.png")))
               ((player-layer 'add-drawable) bmp)
               bmp)))

         (bullet-tile
           (lambda ()
             (let ((bmp (make-bitmap-tile "../gfx/bullet.png")))
               ((bullet-layer 'add-drawable) bmp)
               bmp)))

         (alien-tile
           (lambda (type)
             (let ((bmp (case type
                          ((0) (invader-death))
                          ((1) (invader-R1))
                          ((2) (invader-B1))
                          ((3) (invader-B2))
                          ((4) (invader-G2))
                          ((5) (invader-G3)))))
               ((alien-layer 'add-drawable) bmp)
               bmp)))

         (animate!
           (lambda (alien-id)
             (alien-id 'set-next!)))

         (draw!
           (lambda (id x y)
             (let ((pos-x (* width  x))
                   (pos-y (* height y)))
               ((id 'set-x!) pos-x)
               ((id 'set-y!) pos-y))))

         (set-game-loop-fun!
           (lambda (proc)
             ((window 'set-update-callback!) proc)))

         (set-key-fun!
           (lambda (proc)
             ((window 'set-key-callback!) proc)))

         (set-key-release-fun!
           (lambda (proc)
             ((window 'set-key-release-callback!) proc)))

         (dispatch
           (lambda (msg)
             (case msg
               ((draw!)     draw!)
               ((animate!)  animate!)
               ((alien-id)  alien-tile)
               ((player-id) player-tile)
               ((bullet-id) bullet-tile)
               ((set-key-fun!)         set-key-fun!)
               ((set-key-release-fun!) set-key-release-fun!)
               ((set-game-loop-fun!)   set-game-loop-fun!)
               (else
                 (raise-arguments-error 'window-adt
                                        "invalid argument"
                                        "given" msg))))))
    ((window 'set-background!) "black")
    dispatch))

