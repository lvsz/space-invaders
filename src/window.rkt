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

         (type-from-id car)
         (tile-from-id cdr)

         (player-id
           (lambda ()
             (let ((tile (make-bitmap-tile "../gfx/player.png"
                                          "../gfx/player-mask.png")))
               ((player-layer 'add-drawable) tile)
               (cons 'player tile))))

         (bullet-id
           (lambda ()
             (let ((tile (make-bitmap-tile "../gfx/bullet.png")))
               ((bullet-layer 'add-drawable) tile)
               (cons 'bullet tile))))

         (alien-id
           (lambda (species-number)
             (let ((tile (if (negative? species-number)
                          (invader-death)
                          (case (modulo species-number 5)
                            ;((0) (invader-death))
                            ((0) (invader-R1))
                            ((1) (invader-B1))
                            ((2) (invader-B2))
                            ((3) (invader-G2))
                            ((4) (invader-G3))))))
               ((alien-layer 'add-drawable) tile)
               (cons 'alien  tile))))

         (animate!
           (lambda (id)
             (let ((tile (tile-from-id id)))
               (tile 'set-next!))))

         (draw!
           (lambda (id x y)
             (let ((tile (tile-from-id id))
                   (pos-x (* width  x))
                   (pos-y (* height y)))
               ((tile 'set-x!) pos-x)
               ((tile 'set-y!) pos-y))))

         (remove!
           (lambda (id)
             (let ((tile (tile-from-id id))
                   (type (type-from-id id)))
               (case type
                 ((alien)  ((alien-layer  'remove-drawable) tile))
                 ((player) ((player-layer 'remove-drawable) tile))
                 ((bullet) ((bullet-layer 'remove-drawable) tile))))))

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
               ((remove!)   remove!)
               ((animate!)  animate!)
               ((alien-id)  alien-id)
               ((player-id) player-id)
               ((bullet-id) bullet-id)
               ((set-key-fun!)         set-key-fun!)
               ((set-key-release-fun!) set-key-release-fun!)
               ((set-game-loop-fun!)   set-game-loop-fun!)
               (else
                 (raise-arguments-error 'window-adt
                                        "invalid argument"
                                        "given" msg))))))
    ((window 'set-background!) "black")
    dispatch))

