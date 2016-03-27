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

(define player-speed 30)
(define bullet-speed 15)
(define bullet-limit  7)
(define reload-timer 300)


;;; abstract data type for the player's ship
;;; id and make-id are objects provided by window.rkt
;;; they are used to generate and save a unique identifier
;;; and graphical representation for every instantiation
(define (player-adt make-id)
  (let*
    ((id (make-id))

     ;; default X and Y positions
     (x (- 1/2 (/ player-width 2)))
     (y 9/10)

     ;; zero for no direction, -1 for left, 1 for right
     (direction 0)

     ;; changes direction for key press and release events
     (direction!
       ;; #f means the key isn't being pressed
       ;; #t means the key is being pressed
       (let ((left  #f)
             (right #f))
         ;; takes a symbol and a boolean
         (lambda (key pressed)
           (case key
             ((left)  (set! left  pressed))
             ((right) (set! right pressed)))
           ;; adds left and right
           ;; so direction is none when both or neither are pressed
           (set! direction (+ (if left -1 0)
                              (if right 1 0))))))

     ;; moves the ship within the window's limits
     (move!
       (let ((difference (* 2 unit-width))
             (left-border 0)
             (right-border (- 1 player-width)))
         (lambda ()
           (cond 
             ;; ship isn't touching right border, and it's going right
             ((and (< x right-border) (= direction 1))
              (set! x (+ x difference)))
             ;; ship isn't touching left border, and it's going left
             ((and (> x left-border) (= direction -1))
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
           ((direction!) direction!)
           ((move!)      move!)
           ((draw!)      draw!)
           (else
             (raise-arguments-error
               'player-adt
               "invalid argument"
               "given" msg))))))
    dispatch))


#|
;;; abstract data type for single bullets, used by the player
;;; designed for every bullet to be created before the game begins
;;; this saves resources because it doesn't have to create
;;; a new bullet every time you call shoot!
(define (bullet-adt make-id)
  (let*
    ;; identifier provided by the window adt
    ((id (make-id))

     ;; X and Y positions
     ;; default X position is outside of the window, changes when shot
     (x 1)
     (y 0)

     ;; state of the bullet
     ;; shouldn't be drawn or damage things when it already exploded
     ;; default is inactive, changes when shot
     (exploded? #t)

     ;; called when shot
     ;; makes the bullet active
     ;; and resets its coordinates
     (reset!
       (lambda (new-x new-y)
         (set! exploded? #f)
         (set! x new-x)
         (set! y new-y)))

     ;; upon hitting target, it moves off the screen again
     (explode!
       (lambda ()
         (set! x 1)
         (set! exploded? #t)))

     ;; while active, the bullets moves upwards
     (move!
       (lambda ()
         (unless exploded?
           (set! y (- y bullet-height))
           ;; inactivates the bullet upon hitting the top border
           (when (< y 0)
             (explode!)))))

     ;; sends draw message with id and coordinates to window adt
     (draw!
       (lambda (window)
         ((window 'draw!) id x y)))

     (dispatch
       (lambda (msg)
         (case msg
           ((x) x)
           ((y) y)
           ((exploded?) exploded?)
           ((explode!)  explode!)
           ((reset!)    reset!)
           ((move!)     move!)
           ((draw!)     draw!)
           (else
             (raise-arguments-error
               'bullet-adt
               "invalid argument"
               "given" msg))))))
    dispatch))
|#


(define (bullet-adt x y make-id window)
  (let*
    ((id (make-id))
     (active? #t)
     (explode!
       (lambda ()
         (set! active? #f)))
     (move!
       (lambda ()
         (when active?
           (set! y (- y bullet-height))
           ;; inactivates the bullet upon hitting the top border
           (when (< y 0)
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
           ((active?) active?)
           ((explode!)  explode!)
           ((move!)     move!)
           ((draw!)     draw!)
           (else
             (raise-arguments-error
               'bullet-adt
               "invalid argument"
               "given" msg))))))
    dispatch))

(define (mfor-each proc mlist)
  (unless (null? mlist)
    (proc (mcar mlist))
    (mfor-each proc (mcdr mlist))))

(define (bullets-adt make-id window)
  (let*
    ((bullets '())
     (bullet-for-each
       (lambda (proc)
         (mfor-each proc bullets)))
     (ready? #t)
     (shoot!
       (lambda (x y)
         (set! bullets (mcons (bullet-adt x y make-id window) bullets))))
      ;;   (let ((new-bullet (mcons (bullet-adt x y make-id) '())))
      ;;     (set-mcdr! last-bullet new-bullet)
      ;;     (displayln bullets)
      ;;     (set! last-bullet (mcdr last-bullet)))))
     (move!
       (lambda ()
         (mfor-each (lambda (b) ((b 'move!))) bullets)))
      ;;   (let loop ((current bullets))
      ;;     (cond
      ;;       ((null? current) (void))
      ;;       (((mcar current) 'active?)
      ;;        (set! bullets current)
      ;;        (mfor-each (lambda (b) (b 'move!)) bullets))
      ;;       (loop (mcdr current))))))
     (draw!
       (lambda ()
         (mfor-each (lambda (b) ((b 'draw!))) bullets)))
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





#|
;;; object that stores the individual bullets created at the start of the game
;;; functions like an object pool, by using a circular data structure
(define (bullets-adt make-id)
  (let*
    ;; initialises every bullet
    ;; build-ring functions like build-vector does
    ((bullets (build-ring
                bullet-limit
                (lambda (_)
                  (bullet-adt make-id))))

     ;; for-each function for the bullet structure
     ;; ignores exploded bullets
     (bullet-for-each
       (lambda (proc)
         (ring-for-each
           (lambda (b)
             (unless (b 'exploded?) (proc b)))
           bullets)))

     ;; checks the current bullet in the structure
     ;; can't shoot a new one while the others are still acive
     (ready?
       (lambda ()
         ((ring-head bullets) 'exploded?)))

     ;; shoots a bullet when ready
     ;; requires x & y coordinates
     ;; and changes the current bullet in the ring to the next one
     (shoot!
       (lambda (x y)
         (when (ready?)
           (((ring-head bullets) 'reset!) x y)
           (ring-next! bullets))))

     ;; calls move! for each bullet
     (move!
       (lambda ()
         (ring-for-each (lambda (b) ((b 'move!))) bullets)))

     ;; calls draw! for each bullet
     (draw!
       (lambda (window)
         (ring-for-each (lambda (b) ((b 'draw!) window)) bullets)))

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
|#


;;; initialises the game
;;; optional parameters for window name and random user input
(define (game-init (name "Main") (random? #f))
  (let* ((window  (window-adt name window-width window-height))
         (player  (player-adt  (window 'player-id)))
         (bullets (bullets-adt (window 'bullet-id) window))
         (aliens  (swarm-adt   (window 'alien-id)))
         (score 0)

         ;; boolean that prevents shooting all bullets at once
         (loaded? #t)

         ;; boolean that tells if the shoot! key is being pressed
         (shooting? #f)

         ;; function called when the player hits shoot!
         (shoot!
           (lambda ()
             (when loaded?
               ;; no more shooting till reloaded
               (set! loaded? #f)
               ;; start coordinates for the bullet
               ;; x calculates the center of the player's ship
               (let* ((x (+ (player 'x)
                            (/ player-width 2)
                            (- bullet-width)))
                      (y (player 'y)))
                 ((bullets 'shoot!) x y)))))

         ;; starts the game
         (start
           (lambda ()
             (let* ((player-time 0)
                    (bullet-time 0)
                    (reload-time 0)
                    (alien-time  0)
                    (game-loop-fun
                      (lambda (delta-t)
                        ;; only used with random input activated
                        ;; used for test purposes
                        (when random?
                          (case (random 10)
                            ((0) (set! shooting? #t))
                            ((1) ((player 'direction!) 'left #t))
                            ((2) ((player 'direction!) 'right #t))
                            ((3) (set! shooting? #f))
                            ((4) ((player 'direction!) 'left #f))
                            ((5) ((player 'direction!) 'right #f))))

                        ;; when shooting, shoot
                        (when shooting?
                          (shoot!))

                        ;; reloads gun
                        (set! reload-time (+ reload-time delta-t))
                        (when (> reload-time reload-timer)
                          (set! loaded? #t)
                          (set! reload-time 0))

                        ;; moves the players ship
                        (set! player-time (+ player-time delta-t))
                        (when (> player-time player-speed)
                          ((player 'move!))
                          ((player  'draw!) window)
                          (set! player-time 0))

                        ;; moves the bullets, and checks for collisions
                        (set! bullet-time (+ bullet-time delta-t))
                        (when (> bullet-time bullet-speed)
                          ;; gets the bounds of the alien swarm
                          ;; so it doesn't check for collisons when not near it
                          (let-values (((top bottom) (aliens 'y-bounds))
                                       ((left right) (aliens 'x-bounds)))
                            ((bullets 'for-each)
                             (lambda (b)
                               (let ((x (b 'x))
                                     (y (b 'y)))
                                 (when (and (>= y bottom)
                                            (<= y top)
                                            (>= x left)
                                            (<= x right))
                                   (let ((shot ((aliens 'shot!) x y)))
                                     ;; shot returns #f when no hits
                                     ;; 0 for hitting but not killing an alien
                                     ;; score to be added for killing an alien
                                     (when shot
                                       (set! score (+ score shot))
                                       ((b 'explode!)))))))))
                          ((bullets 'move!))
                          ((bullets 'draw!))
                          (set! bullet-time 0))

                        ;; moves the aliens
                        (set! alien-time (+ alien-time delta-t))
                        (when (> alien-time (aliens 'speed))
                          ((aliens 'move!))
                          (set! alien-time 0)
                          ((aliens 'draw!) window))))

                    ;; function the processes key press events
                    (key-fun
                      (lambda (key)
                        (case key
                          ((up #\space) (set! shooting? #t))
                          ((left)   ((player 'direction!) 'left #t))
                          ((right)  ((player 'direction!) 'right #t))
                          ((escape) (exit)))))

                    ;; function that processes key release events
                    (release-fun
                      (lambda (key)
                        (case key
                          ((up #\space) (set! shooting? #f))
                          ((left)  ((player 'direction!) 'left #f))
                          ((right) ((player 'direction!) 'right #f))))))
               ((window 'set-game-loop-fun!) game-loop-fun)
               ((window 'set-key-release-fun!) release-fun)
               ((window 'set-key-fun!) key-fun))))

         (dispatch
           (lambda (msg)
             (case msg
               ((start) (start))
               ((score) (displayln score))
               ((exit)  (exit))))))
    dispatch))

;; creates a game object and starts it
(define game (game-init))
(game 'start)

