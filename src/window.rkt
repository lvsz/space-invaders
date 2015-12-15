#lang racket

(require "Graphics.rkt")

(provide window-adt)

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
                          ((1) (make-tile-sequence
                                 (list
                                   (make-bitmap-tile "../gfx/alien1-1.png"
                                                     "../gfx/alien1-1-mask.png")
                                   (make-bitmap-tile "../gfx/alien1-2.png"
                                                     "../gfx/alien1-2-mask.png"))))
                          ((2) (make-tile-sequence
                                 (list
                                   (make-bitmap-tile "../gfx/alien2-1.png"
                                                     "../gfx/alien2-1-mask.png")
                                   (make-bitmap-tile "../gfx/alien2-2.png"
                                                     "../gfx/alien2-2-mask.png"))))
                          ((3) (make-tile-sequence
                                 (list
                                   (make-bitmap-tile "../gfx/alien3-1.png"
                                                     "../gfx/alien3-1-mask.png")
                                   (make-bitmap-tile "../gfx/alien3-2.png"
                                                     "../gfx/alien3-2-mask.png")))))))
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
           (lambda (f)
             ((window 'set-update-callback!) f)))
         (set-key-fun!
           (lambda (f)
             ((window 'set-key-callback!) f)))
         (set-key-release-fun!
           (lambda (f)
             ((window 'set-key-release-callback!) f)))
         (dispatch
           (lambda (msg)
             (case msg
               ((set-game-loop-fun!) set-game-loop-fun!)
               ((set-key-release-fun!) set-key-release-fun!)
               ((set-key-fun!) set-key-fun!)
               ((player-id) player-tile)
               ((bullet-id) bullet-tile)
               ((alien-id)  alien-tile)
               ((animate!) animate!)
               ((draw!) draw!)
               (else (error "unkown-render-argument:" msg))))))
    ((window 'set-background!) "black")
    dispatch))
