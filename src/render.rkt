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
           (lambda ()
             (let ((bmp (make-bitmap-tile "images/alien2-1.png"
                                          "images/alien2-1-mask.png")))
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
           (lambda (msg (opt #f))
             (case msg
               ((set-game-loop-fun!) (set-game-loop-fun! opt))
               ((set-key-release-fun!) (set-key-release-fun! opt))
               ((set-key-fun!) (set-key-fun! opt))
               ((rocket-id) (rocket-tile))
               ((bullet-id) (bullet-tile))
               ((alien-id)  (alien-tile))
               ((draw!) (draw! (first opt) (second opt) (third opt)))
               (else (error "unkown-render-argument:" msg))))))
    ((window 'set-background!) "black")
    dispatch))

