#lang racket

(require "Graphics.rkt")

(provide render-init)

(define (render-init name width height)
  (let* ((window (make-window width height name))
         (rocket-layer (window 'make-layer))
         (bullet-layer (window 'make-layer))
         (alien-layer  (window 'make-layer))
         (rocket-tile
           (lambda ()
             (let ((bmp (make-bitmap-tile "images/rocket.png"
                                          "images/rocket-mask.png")))
               ((rocket-layer 'add-drawable) bmp)
               bmp)))
         (bullet-tile
           (lambda ()
             (let ((bmp (make-bitmap-tile "images/bullet.png")))
               ((bullet-layer 'add-drawable) bmp)
               bmp)))
         (alien-tile
           (lambda (type)
             (let ((bmp (case type
                          ((1) (make-bitmap-tile "images/alien1-1.png"
                                                 "images/alien1-1-mask.png"))
                          ((2) (make-bitmap-tile "images/alien2-1.png"
                                                 "images/alien2-1-mask.png"))
                          ((3) (make-bitmap-tile "images/alien3-1.png"
                                                 "images/alien3-1-mask.png")))))
               ((alien-layer 'add-drawable) bmp)
               bmp)))
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
           (lambda (msg . opt)
             (case msg
               ((set-game-loop-fun!) (apply set-game-loop-fun! opt))
               ((set-key-release-fun!) (apply set-key-release-fun! opt))
               ((set-key-fun!) (apply set-key-fun! opt))
               ((rocket-id) (rocket-tile))
               ((bullet-id) (bullet-tile))
               ((alien-id)  (apply alien-tile opt))
               ((draw!) (apply draw! opt))
               (else (error "unkown-render-argument:" msg))))))
    ((window 'set-background!) "black")
    dispatch))

