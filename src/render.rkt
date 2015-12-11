#lang racket

(require "Graphics.rkt")
(require (prefix-in ring: "ring.rkt"))

(provide render-init)

(define (render-init name width height (limit 10))
  (let* ((window (make-window width height name))
         (rocket-layer (window 'make-layer))
         (rocket-tile
           (make-bitmap-tile "images/rocket.png" "images/rocket-mask.png"))
         (draw-rocket!
           (lambda (rocket)
             (let ((pos-x (* width  (rocket 'pos-x)))
                   (pos-y (* height (rocket 'pos-y))))
               ((rocket-tile 'set-x!) pos-x)
               ((rocket-tile 'set-y!) pos-y))))
         (bullets (ring:new limit))
         (bullet-layer (window 'make-layer))
         (add-bullet!
           (lambda (bullet-id)
             (let ((bullet-tile (make-bitmap-tile "images/bullet.png"
                                                  "images/bullet-mask.png")))
               (ring:add! bullets (cons bullet-id bullet-tile))
               ((bullet-layer 'add-drawable) bullet-tile))))
         (take-bullet
           (lambda (bullet-id)
             (cdr (ring:assoc bullet-id bullets))))
         (draw-bullet!
           (lambda (bullet-adt)
             (let ((pos-x (* width  (bullet-adt 'pos-x)))
                   (pos-y (* height (bullet-adt 'pos-y)))
                   (tile  (take-bullet (bullet-adt 'id))))
               ((tile 'set-x!) pos-x)
               ((tile 'set-y!) pos-y))))
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
               ((add-bullet!) (add-bullet! opt))
               ((draw-rocket!) (draw-rocket! opt))
               ((draw-bullet!) (draw-bullet! opt))
               (else (error "unkown-render-argument:" msg))))))
    ((window 'set-background!) "black")
    ((rocket-layer 'add-drawable) rocket-tile)
    dispatch))

