#lang racket

(require "window.rkt"
         "invaders.rkt")

(provide (all-defined-out))

;; unit-width and height provided by window.rkt
(define player-width  (* 13 unit-width))
(define player-height (*  8 unit-height))
(define bullet-width  (*  1 unit-width))
(define bullet-height (*  3 unit-height))
(define menu-item-width  (* 52 unit-width))
(define menu-item-height (* 12 unit-height))
(define pointer-width    (* 7 unit-width))
(define pointer-height   menu-item-height)

;; time between updates in miliseconds
(define player-speed 30)
(define bullet-speed 15)

;; time it takes for the player to reload in miliseconds
(define reload-timer 300)

;; modifies the rate at which the invaders will shoot after moving
;; requires an integer
;; 0 or smaller means 0% chance of a shot
;; 100 or larger means 100% chance of a shot
(define invader-shoot-chance 50)

;;; abstract data type for the player's ship
;;; id and make-id are objects provided by window.rkt
;;; they are used to generate and save a unique identifier
;;; and graphical representation for every instantiation
(define (player-adt make-id)
  (let*
    ((id (make-id))

     ;; player lives
     ;; game over when hit with 0 left
     (lives 3)

     ;; default X and Y positions
     (x (- 1/2 (/ player-width 2)))
     (y 9/10)

     ;; bounds of the player's hitbox
     ;; gets called for collision checking
     (x-bounds
       (lambda ()
         (values x (+ x player-width))))
     (y-bounds
       (lambda ()
         (values (+ y player-height) y)))

     ;; gets called when hit by a bullet
     ;; returns 2 values
     ;; first one if it hit (currently can't return false)
     ;; second one is true if the game ends, false otherwise
     (shot!
       (lambda (x y)
         (if (zero? lives)
           ; bullet hit and game over
           (values #t #t) 
           ; bullet hit, lost a life, game continues
           (begin
             (set! lives (- lives 1))
             (values #t #f)))))

     ;; moves the ship within the window's limits
     ;; only takes the symbols 'left and 'right as arguments
     ;; others get ignored
     (move!
       (let ((difference (* 2 unit-width))
             (left-border 0)
             (right-border (- 1 player-width)))
         (lambda (direction)
           (cond 
             ; ship isn't touching right border, and it's going right
             ((and (eq? direction 'right) (< x right-border))
              (set! x (+ x difference)))
             ; ship isn't touching left border, and it's going left
             ((and (eq? direction 'left) (> x left-border))
              (set! x (- x difference)))))))

     ;; sends draw message with id and coordinates to window adt
     (draw!
       (lambda (window)
         ((window 'draw!) id x y)))

     (dispatch
       (lambda (msg)
         (case msg
           ((x) x)
           ((y) y)
           ((x-bounds) (x-bounds))
           ((y-bounds) (y-bounds))
           ((shot!)      shot!)
           ((move!)      move!)
           ((draw!)      draw!)
           (else
             (raise-arguments-error
               'player-adt
               "invalid argument"
               "given" msg))))))
    dispatch))


;;; bullet adt
;;; type can either be player or invader
;;; also requires coordinates and id gemerator
;;; window is needed to remove the bullet's image after it hit something
(define (bullet-adt type x y make-id window)
  (let*
    ((id (make-id))

     ;; when true, the bullet moves and gets drawn
     ;; when false, the data structure it's located in can safely delete it
     (active #t)

     ;; makes it inactive and removes it from the window
     (explode!
       (lambda ()
         (set! active #f)
         ((window 'remove!) id)))

     ;; +/- decides whether the bullet's moving up or down
     ;; player bullets move up, others down
     (+/- (if (eq? type 'player) - +))

     ;; moves the bullet when active
     (move!
       (lambda ()
         (when active
           (set! y (+/- y bullet-height))
           ; inactivates the bullet upon hitting the top border
           (unless (< 0 y 1)
             (explode!)))))

     ;; sends draw message with id and coordinates to window adt
     (draw!
       (lambda ()
         ((window 'draw!) id x y)))

     (dispatch
       (lambda (msg)
         (case msg
           ((x) x)
           ((y) y)
           ((type)      type)
           ((active?)   active)
           ((explode!)  explode!)
           ((move!)     move!)
           ((draw!)     draw!)
           (else
             (raise-arguments-error
               'bullet-adt
               "invalid argument"
               "given" msg))))))
    dispatch))


;;; some helper functions for the bullets-adt, which relies on mutable lists
;;; may get moved to a seperate module because they're generic

;; for-each function for mutable lists
(define (mfor-each proc . mlists)
  (when (and (pair? mlists) (andmap mpair? mlists))
    (apply proc (map mcar mlists))
    (apply mfor-each proc (map mcdr mlists))))

;; destructive append for mutable lists
(define (mappend! mlist element)
  (if (null? (mcdr mlist))
    (set-mcdr! mlist (mcons element '()))
    (mappend! (mcdr mlist) element)))


;;; the bullets adt is a data structure that holds and modifies every bullet
;;; the make-id function is needed to generate new bullets
;;; window is needed to clear their graphics
(define (bullets-adt make-id window)
  (let*
    (;; starts by making a headed mutable list
     (bullets (mcons 'bullets '()))

     ;; a for-each function that only needs a procedure
     (bullet-for-each
       (lambda (proc)
         (mfor-each proc (mcdr bullets))))

     ;; shoot! creates a new bullet and adds it to the structure
     ;; type can be either player or invader
     (shoot!
       (lambda (type x y)
         (mappend! bullets (bullet-adt type x y make-id window))))

     ;; goes through all bullets till it finds an active one
     ;; the ones before get removed from the list
     (clean-up!
       (lambda ()
         (let loop ((active-bullets (mcdr bullets)))
           (cond
             ((null? active-bullets)
              (set-mcdr! bullets '()))
             (((mcar active-bullets) 'active?)
              (set-mcdr! bullets active-bullets))
             (else
              (loop (mcdr active-bullets)))))))

     ;; move! first tries to clean up any inactive bullets
     ;; then calls move! on the remaining one
     (move!
       (lambda ()
         ; make sure there are any bullets to move
         (when (mpair? (mcdr bullets))
           (clean-up!)
           (bullet-for-each (lambda (b) ((b 'move!)))))))

     ;; draws! all bullets
     (draw!
       (lambda ()
         (bullet-for-each (lambda (b) ((b 'draw!))))))

     (dispatch
       (lambda (msg)
         (case msg
           ((for-each) bullet-for-each)
           ((shoot!)   shoot!)
           ((move!)    move!)
           ((draw!)    draw!)
           (else
             (raise-arguments-error
               'bullets-adt
               "invalid argument"
               "given" msg))))))
    dispatch))


;;; generates everything needed to start a new game
;;; can return a game-loop and key functions
(define (new-game window random?)
  (let*
    ((player  (player-adt  (window 'player-id)))
     (bullets (bullets-adt (window 'bullet-id) window))
     (invaders  (swarm-adt   (window 'invader-id)))

     ;; keeps the score of the current game
     (score 0)

     ;; boolean that tells if the shoot! key is being pressed
     (shooting #f)

     ;; boolean that prevents shooting all bullets at once
     (loaded #t)

     ;; when called, generates a new bullet for the player
     (shoot!
       (lambda ()
         (when loaded
           ;; no more shooting till reloaded
           (set! loaded #f)
           ;; start coordinates for the bullet
           ;; x calculates the center of the player's ship
           (let* ((x (+ (player 'x)
                        (/ player-width 2)
                        (- bullet-width)))
                  (y (player 'y)))
             ((bullets 'shoot!) 'player x y)))))

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

         ;; when shooting, shoot
         (when shooting
           (shoot!))

         ;; reload gun
         (set! reload-time (+ reload-time delta-t))
         (when (and (not loaded) (> reload-time reload-timer))
           (set! loaded #t)
           (set! reload-time 0))

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
              (let* ((x (b 'x))
                     (y (b 'y))
                     (type (b 'type))
                     (target (if (eq? type 'player)
                               invaders
                               player)))
                ;; gets bounds of target (player or invader swarm)
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
                          (exit)))))))))
           ((bullets 'move!))
           ((bullets 'draw!))
           (set! bullet-time 0))))

     ;; method for clearing all game objects from the screen
     (clear!
       (window 'clear-game!))

     (dispatch
       (lambda (msg)
         (case msg
           ((game-loop) game-loop)
           ((input!) input!)
           ((clear!) clear!)
           (else
             (raise-arguments-error
               'game-loop
               "invalid argument"
               "given" msg))))))
    dispatch))


;;; creates a simple menu with 2 options
(define (menu-adt window options)
  (let*
    ((pointer ((window 'pointer-id)))
     (start   ((window 'start-id)))
     (exit    ((window 'exit-id)))
     (menu    ((window 'menu-id)))

     ;; coordinates for menu items
     (pointer-x 1/4)
     (pointer-y 2/5)
     (start-x   1/3)
     (start-y   2/5)
     (exit-x    1/3)
     (exit-y    1/2)

     (position 0)
     (position!
       (lambda (+/-)
         (set! position (modulo (+/- position 1) (vector-length options)))))

     ;; draw function, draws every item
     (draw!
       (lambda ()
         ((window 'draw!) pointer pointer-x pointer-y)
         ((window 'draw!) start start-x start-y)
         ((window 'draw!) exit exit-x exit-y)))

     ;; functions to interact with the pointer
     ;; should be switched to something more generic when new items get added
     (input!
       (lambda (key)
         (case key
           ((up) (position! -) (set! pointer-y start-y))
           ((down) (position! +) (set! pointer-y exit-y))
           ((#\return) ((vector-ref options position))))))

     ;; clears the menu
     ;; used when starting a game
     (clear!
       (window 'clear-menu!))

     (dispatch
       (lambda (msg)
         (case msg
           ((draw!) draw!)
           ((input!) input!)
           ((clear!) clear!)))))
     dispatch))


;;; main function, creates a window and menu when called
;;; optional parameters for window name and random user input
(define (main (name "Main") (random? #f))
  (let*
    ((window  (window-adt name))

     ;; boolean that changes to #f after user input in menu
     ;; and #t after a call to draw!
     ;; avoids unnecessarily redrawing the menu
     (updated #f)
     (updated? (lambda () updated))

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
         ((menu 'clear!))
         ((window 'set-game-loop-fun!) (game 'game-loop)))

     ;; stops the game by clearing the screen, changing the loop function
     ;; also creates a new menu and deletes the old game
     (define (stop)
         ((game 'clear!))
         ((window 'set-game-loop-fun!) menu-loop)
         (set! updated #f)
         (set! menu (menu-adt window (vector start exit)))
         (set! game (new-game window random?))
         (set! state 'menu))

     ;; creates a menu
     (define menu (menu-adt window (vector start exit)))

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
    ((window 'set-game-loop-fun!)   menu-loop)
    ((window 'set-key-fun!)         key-fun)
    ((window 'set-key-release-fun!) release-fun)))

;; starts game
(main)

