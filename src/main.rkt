#lang racket

(require racket/gui/base
         "window.rkt"
	     "player.rkt"
		 "bullets.rkt"
         "invaders.rkt"
         "bunkers.rkt")

(define game-dir (build-path (find-system-path 'doc-dir) "Space Invaders/"))
(define hi-score-file (build-path game-dir "hi-score"))

;; time between updates in miliseconds
(define player-speed 10)
(define bullet-speed 10)

;; time it takes for the player to reload in miliseconds
(define reload-timer 600)

;; modifies the rate at which the invaders will shoot after moving
;; requires an integer
;; 0 or smaller means 0% chance of a shot
;; 100 or larger means 100% chance of a shot
(define invader-shoot-chance 50)
(define invaders/row    11)
(define invaders/column  5)


;;; generates everything needed to start a new game
;;; can return a game-loop and key functions
(define (new-game window on-finish random?)
  (let*
    ((player   (player-adt ((window 'player-id))))
     (bullets  (bullets-adt (window 'bullet-id)))
     (invaders (swarm-adt   (window 'invader-id) invaders/row invaders/column))
     (bunkers  (bunkers-adt (window 'bunker-id)))
     (lives    (build-list (player 'lives)
                           (lambda (_)
                             ((window 'life-id)))))

     (update-lives
       (lambda ()
         (do ((x (/ player-width 2) (+ x (* player-width 3/2)))
              (life lives (cdr life)))
           ((null? life))
           ((window 'draw!) (car life) x 19/20))))

     ;; keeps the score of the current game
     (score 0)
     (score-id ((window 'score-id) "SCORE: 0"))

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
           ((up #\space #\w) (set! shooting state))
           ((left #\a)  (set! left state))
           ((right #\d) (set! right state)))))

     ;; clear input variables
     ;; useful when pausing the game
     (clear-input!
       (lambda ()
         (set! shooting #f)
         (set! left #f)
         (set! right #f)))

     ;; hide or unhide the game
     (hide
       (lambda ()
         (clear-input!)
         ((window 'hide-game))))
     (unhide
       (window 'unhide-game))

     ;; method for clearing all game objects from the screen
     (clear!
       (window 'clear-game!))

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
           ; on success, shoot and roll again
           ; continue untill you get the same shooting spot twice
           (let loop ((xs '()))
             (when (> invader-shoot-chance (random 100))
               (let-values (((x y) (invaders 'shoot)))
                 (unless (member x xs)
                   ((bullets 'shoot!) 'invader x y)
                   (thread (lambda ()
                             (sleep 1/5)
                             (loop (cons x xs))))))))
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
                        ;; a shot returns 2 values
                        ;; first one is
                        ;; #f if missed
                        ;; #t if it hit the player
                        ;; an integer with points if it hit an invader
                        ;; second is only #t when the player or last invader died
                        (let-values (((shot game-over) ((target 'shot!) x y)))
                          ; shot returns #f when no hits
                          ; score to be added for killing an invader
                          (when shot
                            ((b 'explode!))
                            (if (eq? type 'player)
                              (begin (set! score (+ score shot))
                                     ((window 'remove!) score-id)
                                     (set! score-id ((window 'score-id) (format "SCORE: ~a" score)))
                                     ((window 'draw!) score-id 0 0))
                              (unless (null? lives)
                                (bell)
                                ((window 'remove!) (car lives))
                                (set! lives (cdr lives))
                                (update-lives)))
                            ; if the bullet caused the end of the game
                            ; the side that shot it wins
                            (when game-over
                              (if (eq? type 'player)
                                (on-finish score #t)
                                (on-finish score #f))))))))))))
           ((bullets 'move!))
           ((bullets 'draw!) window)
           ((bunkers 'draw!) window)
           (set! bullet-time 0))))

     (dispatch
       (lambda (msg)
         (case msg
           ((input!) input!)
           ((hide)   hide)
           ((unhide) unhide)
           ((clear!) clear!)
           ((game-loop) game-loop)
           (else
             (raise-arguments-error
               'game-loop
               "invalid argument"
               "given" msg))))))
    (update-lives)
    dispatch))

;;; very similar to the code above, but for multiplayer
(define (new-mp-game window on-finish random?)
  (let*
    ((player-1 (player-adt  ((window 'player-id) "cyan") 'player-1 1))
     (player-2 (player-adt  ((window 'player-id) "magenta") 'player-2 2))
     (bullets  (bullets-adt (window 'bullet-id)))
     (invaders (swarm-adt   (window 'invader-id) (* 2 invaders/row) invaders/column #t))
     (bunkers  (bunkers-adt (window 'bunker-id) 9))
     (lives-1  (build-list
                 (player-1 'lives)
                 (lambda (_)
                   ((window 'life-id) "cyan"))))
     (lives-2  (build-list
                 (player-2 'lives)
                 (lambda (_)
                   ((window 'life-id) "magenta"))))

     (update-lives
       (lambda (lives base-x)
         (do ((x (+ base-x (/ player-width 2)) (+ x (* player-width 3/2)))
              (life lives (cdr life)))
           ((null? life))
           ((window 'draw!) (car life) x 19/20))))

     ;; keeps the score of the current game
     (score-1 0)
     (score-2 0)
     (score-1-id ((window 'score-id) "PLAYER 1: 0"))
     (score-2-id ((window 'score-id) "PLAYER 2: 0"))

     ;; boolean that tells if the shoot! key is being pressed
     (shooting-1 #f)
     (shooting-2 #f)

     ;; boolean that prevents shooting all bullets at once
     (loaded-1 #t)
     (loaded-2 #t)

     ;; when called, generates a new bullet for the player
     (shoot!
       (lambda (player who)
         ;; no more shooting till reloaded
         ;; start coordinates for the bullet
         ;; x calculates the center of the player's ship
         (let* ((x (+ (player 'x)
                      (/ player-width 2)
                      (- bullet-width)))
                (y (player 'y)))
           ((bullets 'shoot!) who x y))))

     ;; booleans to indicate player direction
     (left-1 #f)
     (right-1 #f)
     (left-2 #f)
     (right-2 #f)

     ;; functions to be called on user input
     ;; changes the values that control the action of the player's ship
     (input!
       (lambda (key state)
         (case key
           ((#\w)   (set! shooting-1 state))
           ((up)    (set! shooting-2 state))
           ((#\a)   (set! left-1 state))
           ((left)  (set! left-2 state))
           ((#\d)   (set! right-1 state))
           ((right) (set! right-2 state)))))

     ;; clear input variables
     ;; useful when pausing the game
     (clear-input!
       (lambda ()
         (set! shooting-1 #f)
         (set! left-1 #f)
         (set! right-1 #f)
         (set! shooting-2 #f)
         (set! left-2 #f)
         (set! right-2 #f)))

     ;; hide or unhide the game
     (hide
       (lambda ()
         (clear-input!)
         ((window 'hide-game))))
     (unhide
       (window 'unhide-game))

     ;; method for clearing all game objects from the screen
     (clear!
       (window 'clear-game!))

     (detect-colission
       (lambda (bullet target)
         (let-values (((x y) (values (bullet 'x) (bullet 'y)))
                      ((top bottom) (target 'y-bounds))
                      ((left right) (target 'x-bounds)))
           (when (and (>= y bottom)
                      (<= y top)
                      (>= x left)
                      (<= x right))
             ;; a shot returns 2 values
             ;; first one is
             ;; #f if missed
             ;; #t if it hit the player
             ;; an integer with points if it hit an invader
             ;; second is only #t when the player or last invader died
             (let-values (((shot game-over) ((target 'shot!) x y)))
               ; shot returns #f when no hits
               ; score to be added for killing an invader
               (when shot
                 ((bullet 'explode!))
                 (cond 
                   ((eq? (bullet 'type) 'player-1)
                    (set! score-1 (+ score-1 shot))
                    ((window 'remove!) score-1-id)
                    (set! score-1-id ((window 'score-id) (format "PLAYER 1: ~a" score-1)))
                    ((window 'draw!) score-1-id 0 0))
                   ((eq? (bullet 'type) 'player-2)
                    (set! score-2 (+ score-2 shot))
                    ((window 'remove!) score-2-id)
                    (set! score-2-id ((window 'score-id) (format "PLAYER 2: ~a" score-2)))
                    ((window 'draw!) score-2-id 1 0))
                   ((eq? (target 'name) 'player-1)
                    (bell)
                    ((window 'remove!) (car lives-1))
                    (set! lives-1 (cdr lives-1))
                    (update-lives lives-1 0))
                   ((eq? (target 'name) 'player-2)
                    (bell)
                    ((window 'remove!) (car lives-2))
                    (set! lives-2 (cdr lives-2))
                    (update-lives lives-2 1)))
                 ; if the bullet caused the end of the game
                 ; the side that shot it wins
                 (when game-over
                   (if (eq? (bullet 'type) 'invader)
                     (unless (or (player-1 'alive?) (player-2 'alive?))
                       (on-finish score-1 score-2 #f))
                     (on-finish score-1 score-2 #t)))))))))

     ;; variables to keep track of which objects
     ;; can be moved or drawn when the game-loop gets called
     ;; initialize to big enough values to make sure they're drawn on first try
     (reload-time-1 0)
     (reload-time-2 0)
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
             ((0) (set! shooting-2 #t))
             ((1) (set! left-2 #t))
             ((2) (set! right-2 #t))
             ((3) (set! shooting-2 #f))
             ((4) (set! left-2 #f))
             ((5) (set! right-2 #f))))

         ;; reload gun
         (set! reload-time-1 (+ reload-time-1 delta-t))
         (when (and (not loaded-1) (> reload-time-1 reload-timer))
           (set! loaded-1 #t))
         (set! reload-time-2 (+ reload-time-2 delta-t))
         (when (and (not loaded-2) (> reload-time-2 reload-timer))
           (set! loaded-2 #t))

         ;; when shooting, shoot
         (when (and loaded-1 shooting-1 (player-1 'alive?))
           (set! loaded-1 #f)
           (set! reload-time-1 0)
           (shoot! player-1 'player-1))
         (when (and loaded-2 shooting-2 (player-2 'alive?))
           (set! loaded-2 #f)
           (set! reload-time-2 0)
           (shoot! player-2 'player-2))

         ;; moves the players ship
         ;; checks the values of left and right to decide direction
         (set! player-time (+ player-time delta-t))
         (when (> player-time player-speed)
           ((player-1 'move!) (if left-1
                                (unless right-1 'left)
                                (when right-1 'right))
                              0
                              (if (player-2 'alive?)
                                (player-2 'x)
                                2))
           ((player-1 'draw!) window)
           ((player-2 'move!) (if left-2
                                (unless right-2 'left)
                                (when right-2 'right))
                              (if (player-1 'alive?)
                                (+ (player-1 'x) player-width)
                                0)
                              2)
           ((player-2 'draw!) window)
           (set! player-time 0))

         ;; moves the invaders and controls their bullets
         (set! invader-time (+ invader-time delta-t))
         (when (> invader-time (invaders 'speed))
           ((invaders 'move!))
           ; generates a random integer
           ; success varies from 0 to 100% depending on invader-shoot-chance
           ; on success, shoot and roll again
           ; continue untill you get the same shooting spot twice
           (let loop ((xs '()))
             (when (> (* invader-shoot-chance 3/2) (random 100))
               (let-values (((x y) (invaders 'shoot)))
                 (unless (member x xs)
                   ((bullets 'shoot!) 'invader x y)
                   (thread (lambda ()
                             (sleep 1/5)
                             (loop (cons x xs))))))))
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
                       (type (b 'type)))
                  (if ((bunkers 'shot!) x y (if (eq? type 'invader) 'down 'up))
                    ;; explode if a bunker got hit
                    ((b 'explode!))
                    ;; else get bounds of other targets and check for a hit
                  (case type
                    ((player-1 player-2) (detect-colission b invaders))
                    ((invader) (detect-colission b player-1)
                               (detect-colission b player-2))))))))
           ((bullets 'move!))
           ((bullets 'draw!) window)
           ((bunkers 'draw!) window)
           (set! bullet-time 0))))

     (dispatch
       (lambda (msg)
         (case msg
           ((input!) input!)
           ((hide)   hide)
           ((unhide) unhide)
           ((clear!) clear!)
           ((game-loop) game-loop)
           (else
             (raise-arguments-error
               'game-loop
               "invalid argument"
               "given" msg))))))
    ((window 'draw!) score-1-id 0 0)
    ((window 'draw!) score-2-id 1 0)
    (update-lives lives-1 0)
    (update-lives lives-2 1)
    dispatch))


;; simple struct to store menu-items
(struct item (name proc))

;;; creates a simple menu from the givem items
(define (menu-adt window hi-score . items)
  (let*
    ((pointer-id ((window 'item-id) '>))
     (hi-score-id ((window 'item-id) (format "HI-SCORE: ~a" hi-score)))
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
         ((window 'draw!) hi-score-id 0 0)
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
     (game void)

     ;; boolean that changes to #f after user input in menu
     ;; and #t after a call to draw!
     ;; avoids unnecessarily redrawing the menu
     (updated #f)
     (updated? (lambda () updated))

     ;; to see if menu needs to change to pause-menu
     (first-pause #t)

     ;; keeps track of the current state (e.g. menu or game)
     (state 'menu)
     
     (hi-score 
       (if (file-exists? hi-score-file)
         (call-with-input-file hi-score-file (lambda (in) (read in)))
         0)))

    (define (menu-loop delta-t)
      (unless (updated?)
        (set! updated #t)
        ((menu 'draw!))))

    ;; starts the game by clearing the menu and changing the loop function
    (define (start)
      (set! state 'game)
      ((menu 'hide))
      ((window 'singleplayer))
      (set! game (new-game window on-finish random?))
      ((window 'set-game-loop-fun!) (game 'game-loop)))

    (define (multiplayer)
      (set! state 'game)
      ((menu 'hide))
      ((window 'multiplayer))
      (set! game (new-mp-game window on-finish-mp random?))
      ((window 'set-game-loop-fun!) (game 'game-loop)))

    (define (continue)
      (set! state 'game)
      ((menu 'hide))
      ((game 'unhide))
      ((window 'set-game-loop-fun!) (game 'game-loop)))

    ;; stops the game by clearing the screen, changing the loop function
    ;; also creates a new menu and deletes the old game
    (define (pause)
      ((game 'hide))
      ((menu 'unhide))
      (when first-pause
      ((menu 'clear!))
        (set! first-pause #f)
        (set! menu (menu-adt window
                             hi-score
                             (item 'CONTINUE continue)
                             (item 'QUIT     quit)
                             (item 'ZOOM_IN  zoom-in)
                             (item 'ZOOM_OUT zoom-out))))
      ((window 'set-game-loop-fun!) menu-loop)
      (set! updated #f)
      (set! state 'menu))

    (define (quit)
      ((window 'clear-menu!))
      ((window 'unhide-game))
      ((window 'clear-game!))
      (set! menu (default-menu))
      (set! first-pause #t)
      (set! updated #f))

    ;; functions that resize the window
    (define (zoom-in)
      ((window 'scale!) +))
    (define (zoom-out)
      ((window 'scale!) -))

    ;; creates a menu
    (define (default-menu)
      (menu-adt window
                hi-score
                (item 'START start)
                (item 'MULTIPLAYER multiplayer)
                (item 'EXIT  exit)
                (item 'ZOOM_IN zoom-in)
                (item 'ZOOM_OUT zoom-out)))
    (define menu (default-menu))

    ;; what happens when the game's over
    (define (on-finish score victory?)
      (when (> score hi-score)
        (set! hi-score score)
        (unless (directory-exists? game-dir)
          (make-directory game-dir))
        (call-with-output-file
          hi-score-file
          (lambda (out) (write score out))
          #:mode 'text
          #:exists 'replace))
      ((game 'clear!))
      ((menu 'unhide))
      ((menu 'clear!))
      ((window 'set-game-loop-fun!) menu-loop)
      (set! state 'finish)
      (let ((text-id  ((window 'text-id) (if victory? "VICTORY" "YOU LOSE")))
            (score-id ((window 'text-id) (format "SCORE: ~a" score))))
          ((window 'draw!) text-id  1/4 4/10)
          ((window 'draw!) score-id 1/4 5/10)))

    (define (on-finish-mp score-1 score-2 victory?)
      ((game 'clear!))
      ((menu 'unhide))
      ((menu 'clear!))
      ((window 'set-game-loop-fun!) menu-loop)
      (set! state 'finish)
      (let* ((winner (cond ((> score-1 score-2) "CYAN WINS!")
                           ((< score-1 score-2) "MAGENTA WINS!")
                           (else "DRAW!")))
             (score-1-id ((window 'text-id) (format "CYAN: ~a" score-1)))
             (score-2-id ((window 'text-id) (format "MAGENTA: ~a" score-2)))
             (winner-id  ((window 'text-id) winner)))
          ((window 'draw!) score-1-id 1/2 4/12)
          ((window 'draw!) score-2-id 1/2 5/12)
          ((window 'draw!) winner-id  1/2 6/12)))

    (define (restart)
      ((window 'clear-text!))
      (set! state 'menu)
      (set! updated #f)
      (set! first-pause #t)
      (set! menu (default-menu)))

    ;; function that processes key press events
    ;; results depend on current state
    (define (key-fun key)
      (case state
        ((game) (case key
                  ((escape) (pause))
                  (else ((game 'input!) key #t))))
        ((menu) (case key
                  ((escape) (exit))
                  ((up down #\return)
                   (set! updated #f)
                   ((menu 'input!) key))))
        ((finish) (case key
                    ((escape #\return) (restart))))))

    ;; function that processes key release events
    ;; currently only usefull in game mode
    (define (release-fun key)
      (when (eq? state 'game)
        ((game 'input!) key #f)))

    ;; calls these functions to start
    ((window 'set-game-loop-fun!)   menu-loop)
    ((window 'set-key-fun!)         key-fun)
    ((window 'set-key-release-fun!) release-fun)))

;; starts game
(main)

