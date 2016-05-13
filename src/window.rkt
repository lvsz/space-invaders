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

;; with standard settings, 1 unit = 1 pixel
(define unit-width  1/224)
(define unit-height 1/256)

;;; adt for the window, which connects the game to the graphics library
;;; requires a name and optionally non-standard dimensions
(define (window-adt name (width window-width) (height window-height))
  (let*
    ((window (make-window width height name))
     (menu-layer     (window 'make-layer))
     (player-layer   (window 'make-layer))
     (bullet-layer   (window 'make-layer))
     (invader-layer  (window 'make-layer))
     (bunker-layer   (window 'make-layer))
     (game-layers (list player-layer
                        bullet-layer
                        invader-layer
                        bunker-layer))

     ;; accessors for ids
     (type-from-id car)
     (tile-from-id cdr)

     ;; ids are cons cells with their type and bitmap-tile
     ;; id for menu background
     (menu-id
       (lambda ()
         (let ((tile (make-tile 224 256 #f #f)))
           ((tile 'draw-rectangle) 0 0 224 256 "black")
           ((menu-layer 'add-drawable) tile)
           (cons 'menu tile))))

     ;; id for menu pointer
     (pointer-id
       (lambda ()
         (let ((tile (text-tile ">")))
           ((menu-layer 'add-drawable) tile)
           (cons 'menu tile))))

     ;; id for the menu's start item
     (start-id
       (lambda ()
         (let ((tile (text-tile "START")))
           ((menu-layer 'add-drawable) tile)
           (cons 'menu tile))))

     ;; id for the menu's exit item
     (exit-id
       (lambda ()
         (let ((tile (text-tile "EXIT")))
           ((menu-layer 'add-drawable) tile)
           (cons 'menu tile))))

     ;; id for the player's ship
     (player-id
       (lambda ()
         (let ((tile (player-tile)))
           ((player-layer 'add-drawable) tile)
           (cons 'player tile))))

     ;; id for bullets
     (bullet-id
       (lambda ()
         (let ((tile (bullet-tile)))
           ((bullet-layer 'add-drawable) tile)
           (cons 'bullet tile))))

     ;; id for an invader
     ;; requires an integer to specify what graphic to use
     (invader-id
       (lambda (species-number (explosion #f))
         (let ((tile (if explosion
                       (case (modulo species-number 5)
                         ((1 2) (explosion-tile "cyan"))
                         ((3 4) (explosion-tile "green"))
                         ((5 0) (explosion-tile "magenta")))
                       (case (modulo species-number 5)
                         ((0) (invader-a "magenta"))
                         ((1) (invader-a "cyan"))
                         ((2) (invader-b "cyan"))
                         ((3) (invader-b "green"))
                         ((4) (invader-c "green"))
                         ((5) (invader-c "magenta"))))))
           ((invader-layer 'add-drawable) tile)
           (cons 'invader  tile))))

     (bunker-id
       (lambda (bitmap width height)
         (let ((tile (bunker-tile bitmap width height)))
           ((bunker-layer 'add-drawable) tile)
           (cons 'bunker tile))))

     ;; animates bitmap-tiles that support it
     (animate!
       (lambda (id)
         (let ((tile (tile-from-id id)))
           (tile 'set-next!))))

     ;; generic draw method
     ;; requires the id and coordinates
     (draw!
       (lambda (id x y)
         (let ((tile (tile-from-id id))
               (pos-x (* width  x))
               (pos-y (* height y)))
           ((tile 'set-x!) pos-x)
           ((tile 'set-y!) pos-y))))

     ;; removes a bitmap-tile from the screen
     (remove!
       (lambda (id)
         (let ((tile (tile-from-id id))
               (type (type-from-id id)))
           (case type
             ((menu)    ((menu-layer    'remove-drawable) tile))
             ((invader) ((invader-layer 'remove-drawable) tile))
             ((player)  ((player-layer  'remove-drawable) tile))
             ((bullet)  ((bullet-layer  'remove-drawable) tile))
             ((bunker)  ((bunker-layer  'remove-drawable) tile))))))

     ;; sets the main loop function
     (set-game-loop-fun!
       (lambda (proc)
         ((window 'set-update-callback!) proc)))

     ;; sets the key press function
     (set-key-fun!
       (lambda (proc)
         ((window 'set-key-callback!) proc)))

     ;; sets the key release function
     (set-key-release-fun!
       (lambda (proc)
         ((window 'set-key-release-callback!) proc)))

     (hide-menu
       (menu-layer 'hide))
     (unhide-menu
       (menu-layer 'unhide))

     (hide-game
       (lambda ()
         (for-each (lambda (layer) ((layer 'hide))) game-layers)))
     (unhide-game
       (lambda ()
         (for-each (lambda (layer) ((layer 'unhide))) game-layers)))

     ;; clears menu layer
     (clear-menu!
       (menu-layer 'clear!))

     ;; clears all game layers
     (clear-game!
       (lambda ()
         (for-each (lambda (layer) ((layer 'clear!))) game-layers)))

     (dispatch
       (lambda (msg)
         (case msg
           ((draw!)      draw!)
           ((remove!)    remove!)
           ((animate!)   animate!)
           ((menu-id)    menu-id)
           ((pointer-id) pointer-id)
           ((start-id)   start-id)
           ((exit-id)    exit-id)
           ((invader-id) invader-id)
           ((player-id)  player-id)
           ((bullet-id)  bullet-id)
           ((bunker-id)  bunker-id)
           ((set-key-fun!)         set-key-fun!)
           ((set-key-release-fun!) set-key-release-fun!)
           ((set-game-loop-fun!)   set-game-loop-fun!)
           ((hide-menu)            hide-menu)
           ((unhide-menu)          unhide-menu)
           ((hide-game)            hide-game)
           ((unhide-game)          unhide-game)
           ((clear-game!)          clear-game!)
           ((clear-menu!)          clear-menu!)
           (else
             (raise-arguments-error 'window-adt
                                    "invalid argument"
                                    "given" msg))))))

    ;; defaults to black background on creation
    ((window 'set-background!) "black")
    dispatch))

