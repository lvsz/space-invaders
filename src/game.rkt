#lang racket

(require "window.rkt"
         "aliens.rkt"
         "ring.rkt")

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

(define player-speed 30)
(define bullet-speed 15)
(define bullet-limit  7)
(define reload-timer 300)
(define alien-shoot-coefficient 15/100)
(define alien-shoot-chance (floor (* 100 (/ alien-shoot-coefficient))))


;;; abstract data type for the player's ship
;;; id and make-id are objects provided by window.rkt
;;; they are used to generate and save a unique identifier
;;; and graphical representation for every instantiation
(define (player-adt make-id)
  (let*
    ((id (make-id))
     (lives 3)

     ;; default X and Y positions
     (x (- 1/2 (/ player-width 2)))
     (y 9/10)
     (x-bounds
       (lambda ()
         (values x (+ x player-width))))
     (y-bounds
       (lambda ()
         (values (+ y player-height) y)))

     (shot!
       (lambda (x y)
         (if (zero? lives)
           (values #t #t)
           (begin
             (set! lives (- lives 1))
             (values #t #f)))))

     ;; moves the ship within the window's limits
     (move!
       (let ((difference (* 2 unit-width))
             (left-border 0)
             (right-border (- 1 player-width)))
         (lambda (direction)
           (cond 
             ;; ship isn't touching right border, and it's going right
             ((and (eq? direction 'right) (< x right-border))
              (set! x (+ x difference)))
             ;; ship isn't touching left border, and it's going left
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


(define (bullet-adt type x y make-id window)
  (let*
    ((id (make-id))
     (active? #t)
     (explode!
       (lambda ()
         (set! active? #f)
         ((window 'remove!) id)))

     (+/- (if (eq? type 'player) - +))
     (move!
       (lambda ()
         (when active?
           (set! y (+/- y bullet-height))
           ;; inactivates the bullet upon hitting the top border
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
           ((active?)   active?)
           ((explode!)  explode!)
           ((move!)     move!)
           ((draw!)     draw!)
           (else
             (raise-arguments-error
               'bullet-adt
               "invalid argument"
               "given" msg))))))
    dispatch))


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


(define (bullets-adt make-id window)
  (let*
    ((bullets (mcons 'bullets '()))
     (bullet-for-each
       (lambda (proc)
         (mfor-each proc (mcdr bullets))))
     (ready? #t)
     (shoot!
       (lambda (type x y)
         (mappend! bullets (bullet-adt type x y make-id window))))
     (move!
       (lambda ()
         (when (mpair? (mcdr bullets))
           (let loop ((active-bullets (mcdr bullets)))
             (cond
               ((null? active-bullets)
                (set-mcdr! bullets '()))
               (((mcar active-bullets) 'active?)
                (set-mcdr! bullets active-bullets)
                (bullet-for-each (lambda (b) ((b 'move!)))))
               (else
                (loop (mcdr active-bullets))))))))
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


(define (new-game window random?)
  (let*
    ((player  (player-adt  (window 'player-id)))
     (bullets (bullets-adt (window 'bullet-id) window))
     (aliens  (swarm-adt   (window 'alien-id)))
     (score 0)

     (left #f)
     (right #f)

     ;; boolean that tells if the shoot! key is being pressed
     (shooting #f)

     ;; boolean that prevents shooting all bullets at once
     (loaded #t)

     ;; function called when the player hits shoot!
     (shoot!
       (lambda ()
           ;; no more shooting till reloaded
           (set! loaded #f)
           ;; start coordinates for the bullet
           ;; x calculates the center of the player's ship
           (let* ((x (+ (player 'x)
                        (/ player-width 2)
                        (- bullet-width)))
                  (y (player 'y)))
             ((bullets 'shoot!) 'player x y))))

     (up!
       (lambda (state)
         (set! shooting state)))
     (left!
       (lambda (state)
         (set! left state)))
     (right!
       (lambda (state)
         (set! right state)))

     (reload-time 0)
     (player-time player-speed)
     (bullet-time bullet-speed)
     (alien-time (aliens 'speed))
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

         ;; reloads gun
         (set! reload-time (+ reload-time delta-t))
         (when (and (not loaded) (> reload-time reload-timer))
           (set! loaded #t)
           (set! reload-time 0))

         ;; moves the players ship
         (set! player-time (+ player-time delta-t))
         (when (> player-time player-speed)
           ((player 'move!) (if left
                              (unless right 'left)
                              (when right 'right)))
           ((player 'draw!) window)
           (set! player-time 0))

         ;; moves the aliens and controls their bullets
         (set! alien-time (+ alien-time delta-t))
         (when (> alien-time (aliens 'speed))
           ((aliens 'move!))
           (when (<= 100 (random alien-shoot-chance))
             (let-values (((x y) (aliens 'shoot)))
               ((bullets 'shoot!) 'alien x y)))
           (set! alien-time 0)
           ((aliens 'draw!) window))

         ;; moves the bullets, and checks for collisions
         (set! bullet-time (+ bullet-time delta-t))
         (when (> bullet-time bullet-speed)
           ;; gets the bounds of the alien swarm
           ;; so it doesn't check for collisons when not near it
           ((bullets 'for-each)
            (lambda (b)
              (let* ((x (b 'x))
                     (y (b 'y))
                     (type (b 'type))
                     (target (if (eq? type 'player)
                               aliens
                               player)))
                (let-values (((top bottom) (target 'y-bounds))
                             ((left right) (target 'x-bounds)))
                  (when (and (>= y bottom)
                             (<= y top)
                             (>= x left)
                             (<= x right))
                    (let-values (((shot game-over) ((target 'shot!) x y)))
                      ;; shot returns #f when no hits
                      ;; 0 for hitting but not killing an alien
                      ;; score to be added for killing an alien
                      (when shot
                        ((b 'explode!))
                        (when (not (eq? shot 0))
                          ((target 'draw!) window)
                          (when (eq? type 'player)
                            (set! score (+ score shot))))
                        (when game-over
                          (displayln (if (eq? type 'player)
                                       "YOU WIN"
                                       "YOU LOSE"))
                          (exit)))))))))
           ((bullets 'move!))
           ((bullets 'draw!))
           (set! bullet-time 0))))

     (clear!
       (window 'clear-game!))

     (dispatch
       (lambda (msg)
         (case msg
           ((game-loop) game-loop)
           ((up!)    up!)
           ((left!)  left!)
           ((right!) right!)
           ((clear!) clear!)
           (else
             (raise-arguments-error
               'game-loop
               "invalid argument"
               "given" msg))))))
    dispatch))


(define (menu-adt window)
  (let*
    ((pointer ((window 'pointer-id)))
     (start   ((window 'start-id)))
     (exit    ((window 'exit-id)))
     (menu    ((window 'menu-id)))
     (pointer-x 1/4)
     (pointer-y 2/5)
     (start-x   1/3)
     (start-y   2/5)
     (exit-x    1/3)
     (exit-y    1/2)
     (draw!
       (lambda ()
         ((window 'draw!) pointer pointer-x pointer-y)
         ((window 'draw!) start start-x start-y)
         ((window 'draw!) exit exit-x exit-y)))
     (up!
       (lambda ()
         (set! pointer-y start-y)))
     (down!
       (lambda ()
         (set! pointer-y exit-y)))
     (enter!
       (lambda (start-f exit-f)
         (if (= pointer-y start-y)
           (start-f)
           (exit-f))))
     (clear!
       (window 'clear-menu!))
     (dispatch
       (lambda (msg)
         (case msg
           ((draw!) draw!)
           ((up!) up!)
           ((down!) down!)
           ((enter!) enter!)
           ((clear!) clear!)))))
     dispatch))


;;; initialises the game
;;; optional parameters for window name and random user input
(define (game-init (name "Main") (random? #f))
  (let*
    ((window  (window-adt name window-width window-height))
     (state 'menu)
     (menu (menu-adt window))
     (updated #f)
     (updated? (lambda () updated))
     (menu-loop
       (lambda (delta-t)
         (unless (updated?)
           (set! updated #t)
           ((menu 'draw!)))))

     (game (new-game window random?))
     (start
       (lambda ()
         (set! state 'game)
         ((menu 'clear!))
         ((window 'set-game-loop-fun!) (game 'game-loop))))
     (stop
       (lambda ()
         ((game 'clear!))
         ((window 'set-game-loop-fun!) menu-loop)
         (set! updated #f)
         (set! menu (menu-adt window))
         (set! game (new-game window random?))
         (set! state 'menu)))

     ;; function that processes key press events
     (key-fun
       (lambda (key)
         (if (eq? state 'game)
           (case key
             ((up #\space) ((game 'up!) #t))
             ((left)    ((game 'left!)  #t))
             ((right)   ((game 'right!) #t))
             ((escape)  (stop)))
           (case key
             ((up)   (set! updated #f) ((menu 'up!)))
             ((down) (set! updated #f) ((menu 'down!)))
             ((#\return) ((menu 'enter!) start exit))
             ((escape) (exit))))))

     ;; function that processes key release events
     (release-fun
       (lambda (key)
         (when (eq? state 'game)
           (case key
             ((up #\space) ((game 'up!) #f))
             ((left)    ((game 'left!)  #f))
             ((right)   ((game 'right!) #f)))))))

    ((window 'set-game-loop-fun!) menu-loop)
    ((window 'set-key-fun!)       key-fun)
    ((window 'set-key-release-fun!) release-fun)))

;; starts game
(game-init)

