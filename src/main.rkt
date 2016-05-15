#lang racket

(require "window.rkt"
	     "player.rkt"
		 "bullets.rkt"
         "invaders.rkt"
         "bunkers.rkt")

;; time between updates in miliseconds
(define player-speed 30)
(define bullet-speed 15)

;; time it takes for the player to reload in miliseconds
(define reload-timer 600)

;; modifies the rate at which the invaders will shoot after moving
;; requires an integer
;; 0 or smaller means 0% chance of a shot
;; 100 or larger means 100% chance of a shot
(define invader-shoot-chance 75)

;;; generates everything needed to start a new game
;;; can return a game-loop and key functions
(define (new-game window random?)
  (let*
    ((player   (player-adt  (window 'player-id)))
     (bullets  (bullets-adt (window 'bullet-id)))
     (invaders (swarm-adt   (window 'invader-id)))
     (bunkers  (bunkers-adt (window 'bunker-id)))

     ;; keeps the score of the current game
     (score 0)

     ;; boolean that tells if the shoot! key is being pressed
     (shooting #f)

     ;; boolean that prevents shooting all bullets at once
     (loaded #t)

     ;; when called, generates a new bullet for the player
     (shoot!
       (lambda ()
         ;; no more shooting till reloaded
         ;; start coordinates for the bullet
         ;; x calculates the center of the player's ship
         (let* ((x (+ (player 'x)
                      (/ player-width 2)
                      (- bullet-width)))
                (y (player 'y)))
           ((bullets 'shoot!) 'player x y))))

     ;; booleans to indicate player direction
     (left #f)
     (right #f)

     ;; functions to be called on user input
     ;; changes the values that control the action of the player's ship
     (input!
       (lambda (key state)
         (case key
           ((up #\space) (set! shooting state))
           ((left) (set! left state))
           ((right) (set! right state)))))

     (clear-input!
       (lambda ()
         (set! shooting #f)
         (set! left #f)
         (set! right #f)))

     ;; variables to keep track of which objects
     ;; can be moved or drawn when the game-loop gets called
     ;; initialize to big enough values to make sure they're drawn on first try
     (reload-time 0)
     (player-time player-speed)
     (bullet-time bullet-speed)
     (invader-time (invaders 'speed))

     ;; the main loop for the game
     ;; requires time passed since last call
     (game-loop
       (lambda (delta-t)
         ;; only used with random input activated
         ;; used for test purposes
         (when random?
           (case (random 10)
             ((0) (set! shooting #t))
             ((1) (set! left #t))
             ((2) (set! right #t))
             ((3) (set! shooting #f))
             ((4) (set! left #f))
             ((5) (set! right #f))))

         ;; reload gun
         (set! reload-time (+ reload-time delta-t))
         (when (and (not loaded) (> reload-time reload-timer))
           (set! loaded #t))

         ;; when shooting, shoot
         (when (and loaded shooting)
           (set! loaded #f)
           (set! reload-time 0)
           (shoot!))

         ;; moves the players ship
         ;; checks the values of left and right to decide direction
         (set! player-time (+ player-time delta-t))
         (when (> player-time player-speed)
           ((player 'move!) (if left
                              (unless right 'left)
                              (when right 'right)))
           ((player 'draw!) window)
           (set! player-time 0))

         ;; moves the invaders and controls their bullets
         (set! invader-time (+ invader-time delta-t))
         (when (> invader-time (invaders 'speed))
           ((invaders 'move!))
           ; generates a random integer
           ; success varies from 0 to 100% depending on invader-shoot-chance
           (when (> invader-shoot-chance (random 100))
             (let-values (((x y) (invaders 'shoot)))
               ((bullets 'shoot!) 'invader x y)))
           (set! invader-time 0)
           ((invaders 'draw!) window))

         ;; moves the bullets, and checks for collisions
         (set! bullet-time (+ bullet-time delta-t))
         (when (> bullet-time bullet-speed)
           ((bullets 'for-each)
            (lambda (b)
              (when (b 'active?)
                (let* ((x (b 'x))
                       (y (b 'y))
                       (type (b 'type))
                       (target (if (eq? type 'player)
                                 invaders
                                 player)))
                  (if ((bunkers 'shot!) x y (if (eq? type 'player) 'up 'down))
                    ;; explode if a bunker got hit
                    ((b 'explode!))
                    ;; else get bounds of other targets and check for a hit
                    (let-values (((top bottom) (target 'y-bounds))
                                 ((left right) (target 'x-bounds)))
                      (when (and (>= y bottom)
                                 (<= y top)
                                 (>= x left)
                                 (<= x right))
                        ;; a shot returns 2 value
                        ;; first one is
                        ;; #f if missed
                        ;; #t if it hit the player
                        ;; an integer with points if it hit an invader
                        ;; second is only #t when the player or last invader died
                        (let-values (((shot game-over) ((target 'shot!) x y)))
                          ; shot returns #f when no hits
                          ; 0 for hitting but not killing an invader
                          ; score to be added for killing an invader
                          (when shot
                            ((b 'explode!))
                            (when (not (eq? shot 0))
                              ((target 'draw!) window)
                              (when (eq? type 'player)
                                (set! score (+ score shot))))
                            ; if the bullet caused the end of the game
                            ; the side that shot it wins
                            (when game-over
                              (displayln (if (eq? type 'player)
                                           "YOU WIN"
                                           "YOU LOSE"))
                              (exit)))))))))))
           ((bullets 'move!))
           ((bullets 'draw!) window)
           ((bunkers 'draw!) window)
           (set! bullet-time 0))))

     (hide
       (window 'hide-game))
     (unhide
       (window 'unhide-game))

     ;; method for clearing all game objects from the screen
     (clear!
       (window 'clear-game!))

     (dispatch
       (lambda (msg)
         (case msg
           ((game-loop) game-loop)
           ((input!) input!)
           ((clear-input!) clear-input!)
           ((hide)  hide)
           ((unhide) unhide)
           ((clear!) clear!)
           (else
             (raise-arguments-error
               'game-loop
               "invalid argument"
               "given" msg))))))
    dispatch))

	
;; simple struct to store menu-items
(struct item (name proc))

;;; creates a simple menu with 2 options
(define (menu-adt window . items)
  (let*
    ((pointer (item  '> void))
     (pointer-id ((window 'item-id) (item-name pointer)))
     (items (list->vector items))
     (ids (vector-map (lambda (item) ((window 'item-id) (item-name item))) items))
     (number-of-items (vector-length items))

     (position 0)
     (position!
       (lambda (direction)
         (set! position
           (modulo (+ position (if (eq? direction 'up) -1 1))
                   number-of-items))))

     ;; draw function, draws every item
     (draw!
       (lambda ()
         (do ((i 0   (+ i 1))
              (y 1/4 (+ y 1/12)))
           ((= i number-of-items) (void))
           (when (= i position)
             ((window 'draw!) pointer-id 1/4 y))
           ((window 'draw!) (vector-ref ids i) 1/3 y))))

     ;; functions to interact with the pointer
     ;; should be switched to something more generic when new items get added
     (input!
       (lambda (key)
         (case key
           ((up down) (position! key))
           ((#\return) ((item-proc (vector-ref items position)))))))

     (hide
       (window 'hide-menu))
     (unhide
       (window 'unhide-menu))

     ;; clears the menu
     ;; used when starting a game
     (clear!
       (window 'clear-menu!))

     (dispatch
       (lambda (msg)
         (case msg
           ((draw!)  draw!)
           ((input!) input!)
           ((hide)   hide)
           ((unhide) unhide)
           ((clear!) clear!)))))
     dispatch))


;;; main function, creates a window and menu when called
;;; optional parameters for window name and random user input
(define (main (name "Main") (random? #f))
  (let*
    ((window (window-adt name))

     ;; boolean that changes to #f after user input in menu
     ;; and #t after a call to draw!
     ;; avoids unnecessarily redrawing the menu
     (updated #f)
     (updated? (lambda () updated))

     ;; to see if menu needs to change to pause-menu
     (first-pause #t)

     ;; keeps track of the current state (e.g. menu or game)
     (state 'menu)

     ;; creates a new game
     (game (new-game window random?)))

    (define (menu-loop delta-t)
      (unless (updated?)
        (set! updated #t)
        ((menu 'draw!))))

    ;; starts the game by clearing the menu and changing the loop function
    (define (start)
      (set! state 'game)
      ((menu 'hide))
      ((game 'unhide))
      ((window 'set-game-loop-fun!) (game 'game-loop)))

    (define continue
      start)

    ;; stops the game by clearing the screen, changing the loop function
    ;; also creates a new menu and deletes the old game
    (define (stop)
      ((game 'hide))
      ((game 'clear-input!))
      ((menu 'unhide))
      (when first-pause
      ((menu 'clear!))
        (set! first-pause #f)
        (set! menu (menu-adt window
                             (item 'CONTINUE continue)
                             (item 'EXIT  exit)
                             (item 'ZOOM_IN zoom-in)
                             (item 'ZOOM_OUT zoom-out))))
      ((window 'set-game-loop-fun!) menu-loop)
      (set! updated #f)
      (set! state 'menu))

    ;; functions that resize the window
    (define (zoom-in)
      ((window 'scale!) +))
    (define (zoom-out)
      ((window 'scale!) -))

    ;; creates a menu
    (define menu (menu-adt window
                           (item 'START start)
                           (item 'EXIT  exit)
                           (item 'ZOOM_IN zoom-in)
                           (item 'ZOOM_OUT zoom-out)))


    ;; function that processes key press events
    ;; results depend on current state
    (define (key-fun key)
      (if (eq? state 'game)
        (case key
          ((escape) (stop))
          (else ((game 'input!) key #t)))
        (case key
          ((escape) (exit))
          (else (set! updated #f) ((menu 'input!) key)))))

    ;; function that processes key release events
    ;; currently only usefull in game mode
    (define (release-fun key)
      (when (eq? state 'game)
        ((game 'input!) key #f)))

    ;; calls these three functions to draw a window with a menu
    ((game 'hide))
    ((window 'set-game-loop-fun!)   menu-loop)
    ((window 'set-key-fun!)         key-fun)
    ((window 'set-key-release-fun!) release-fun)))

;; starts game
(main)

