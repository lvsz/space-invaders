#lang racket

(require "window.rkt")

(provide swarm-adt)

(define invaders/column 5)
(define invaders/row 11)
(define invader-width (* 12 unit-width))
(define invader-height (* 8 unit-height))

;; spacing between invaders when generating the swarm
(define horizontal-spacing (* invader-width 4/3))
(define vertical-spacing   (* invader-height 2))

;; distance invaders move on every update
(define x-move (* 4 unit-width))
(define y-move (* 8 unit-height))

;; generic fold function for vectors
;; ought to work exactly like fold-left in R6RS
(define (vector-fold proc x . vec)
  (let ((end (apply min (map vector-length vec))))
    (let loop ((i 0) (acc x))
      (if (= i end)
        acc
        (loop (+ i 1)
              (apply proc acc (map (lambda (v) (vector-ref v i)) vec)))))))

;; when a bullet collision returns a positive integer, something got killed
(define (kill? x)
  (and (integer? x) (positive? x)))


;;; adt for a single invader object
;;; requires an x-coordinate, identifier, and an integer for invader type
(define (invader-adt y make-id type)
  (let*
    ;; identifier to store state needed by the window adt
    ((id (make-id type))

     (health 1)

     ;; points upon killing are based on initial health
     (points (+ 100 (* 100 type)))

     ;; is it alive?
     (alive?
       (lambda ()
         (positive? health)))

     ;; becomes true when explode graphic gets drawn
     (exploded #f)
     ;; becomes true after removing it from the screen
     (cleared  #f)

     ;; checks if it hit
     ;; false if already dead
     ;; subtracts a health point otherwise
     (hit!
       (lambda ()
         (if (not (alive?))
           #f
           (begin
             (set! health (- health 1))
             ; if it was a kill, return points, otherwise 0
             (if (alive?) 0 points)))))

     ;; moves the invader on the y-axis
     ;; position on the x-axis is stored in the invader-column adt
     (move!
       (lambda ()
         (set! y (+ y y-move))))

     ;; draws and animates the invader
     ;; requires window adt and y coordinate
     (draw!
       (lambda (window x)
         (cond
           ; if alive, just draw and animate
           ((alive?)
            ((window 'draw!) id x y)
            ((window 'animate!) id))
           ; if health is 1, remove invader graphic
           ; then create a new id for the explosion animation
           ; draws the explosion and sets health to 0
           ((not exploded)
            ((window 'remove!) id)
            (set! id (make-id type 'explosion))
            ((window 'draw!) id x y)
            (set! exploded #t))
           ; when health is 0, remove explosion graphic
           ; then change health to -1, for which this conditional returns null
           ((not cleared)
            ((window 'remove!) id)
            (set! cleared #t)))))

     (dispatch
       (lambda (msg)
         (case msg
           ((y) y)
           ((alive?) (alive?))
           ((hit!)   hit!)
           ((move!)  move!)
           ((draw!)  draw!)
           (else
             (raise-arguments-error
               'invader-adt
               "invalid argument"
               "given" msg))))))
    dispatch))


;;; adt that stores a 1 dimensional vector of invaders
;;; requires everything a single invader needs, plus a x coordinate
(define (invader-column x y make-id)
  (let*
    (;; the invaders are stored in a vector
     ;; builds it using the invader-adt from above
     (invaders
       (build-vector
         invaders/column
         (lambda (i)
           (invader-adt (* i vertical-spacing) make-id (- invaders/column i 1)))))

    ;; number of invaders alive, 0 means column is empty
    (alive invaders/column)
     (alive?
       (lambda ()
         (positive? alive)))

     ;; like vector-ref
     (invader-ref
       (lambda (idx)
         (vector-ref invaders idx)))

     ;; top-most active invader in the row
     (top-most  0)

     ;; bottom-most active invader in the row
     ;; default depends on length of row
     (bottom-most (- invaders/column 1))

     ;; updates top-most to new top-most active invader
     (update-top-most!
       (lambda ()
         (let loop ((i (+ top-most 1)))
           (cond (((invader-ref i) 'alive?)
                  (set! top-most i))
                 (else (loop (+ i 1)))))))

     ;; updates bottom-most to new bottom-most active invader
     (update-bottom-most!
       (lambda ()
         (let loop ((i (- bottom-most 1)))
           (cond (((invader-ref i) 'alive?)
                  (set! bottom-most i))
                 (else (loop (- i 1)))))))

     ;; incoming shot
     (shot!
       (lambda (b-y)
         (if (not (alive?))
           #f
           (let loop ((i bottom-most))
             (if (< i top-most)
               #f
               (let* ((invader (invader-ref i))
                      (a-y   (invader 'y)))
                  (if (or (< b-y a-y)
                          (> b-y (+ a-y invader-height)))
                    ; shot missed, continue looking
                    (loop (- i 1))
                    ; returns the result of a hit
                    ; an integer if the invader's still alive
                    ; positive integer if it's also a kill
                    ; #f if it's already dead
                    (let ((result ((invader 'hit!))))
                      (when (kill? result)
                        (set! alive (- alive 1))
                        (cond
                          ((and (= i bottom-most) (alive?))
                           (update-bottom-most!))
                          ((and (= i top-most) (alive?))
                           (update-top-most!))))
                      result))))))))

     ;; moves the invader column within the window limits
     (move!
       (lambda (direction)
         (when (alive?)
           (if (eq? direction 'down)
             ; when moving down, move all invaders
             (vector-map (lambda (a) ((a 'move!))) invaders)
             ; otherwise just change x-coordinate of column
             (set! x ((if (eq? direction 'right) + -) x x-move))))))

     ;; maps draw! on all elements
     (draw!
       (lambda (window)
         (vector-map (lambda (a) ((a 'draw!) window x)) invaders)))

     (dispatch
       (lambda (msg)
         (case msg
           ((x) x)
           ((top-bound)  ((invader-ref top-most) 'y))
           ((bottom-bound) (+ ((invader-ref bottom-most) 'y) invader-height))
           ((alive?) (alive?))
           ((shot!)  shot!)
           ((move!)  move!)
           ((draw!)  draw!)
           (else
             (raise-arguments-error
               'invader-column
               "invalid argument"
               "given" msg))))))
    dispatch))


;;; swarm-adt is a row of invader-columns
(define (swarm-adt make-id)
  (let* ((x 0)
         (y 0)

         (game-over #f)

         ;; builds a vector of invader-column datastructures
         (invaders (build-vector
                   invaders/row
                   (lambda (i)
                     ;; calculates spacing between columns
                     (invader-column (+ x (* i horizontal-spacing)) y make-id))))

         ;; left column
         (left 0)
         (update-left!
           (lambda ()
             (let loop ((i (+ left 1)))
               (if ((vector-ref invaders i) 'alive?)
                 (set! left i)
                 (loop (+ i 1))))))

         ;; right column
         (right (- invaders/row 1))
         (update-right!
           (lambda ()
             (let loop ((i (- right 1)))
               (if ((vector-ref invaders i) 'alive?)
                 (set! right i)
                 (loop (- i 1))))))

         ;; when called, it returns 2 values, an X and Y coordinate
         ;; meant to get coordinates for a new bullet
         (shoot
           (lambda ()
             (let loop ((i (- (random (+ left 1) (+ right 2)) 1)))
               (if ((vector-ref invaders i) 'alive?)
                 (values (+ ((vector-ref invaders i) 'x) (/ invader-width 2))
                         ((vector-ref invaders i) 'bottom-bound))
                 (loop (- (random (+ left 1) (+ right 2)) 1))))))

         ;; intitial speed depends on amount of invaders
         (speed (* invaders/column invaders/row 16))

         ;; changes speed
         (speed!
           (lambda ()
             (set! speed (- speed 8))))

         ;; checks from left to right if any bullet can hit something
         (shot!
           (let ((victory #f))
             (lambda (b-x b-y)
               ;; gets the score value, if any
               ;; no hit is false
               ;; a hit returns an integer
               (let ((result
                       (let loop ((i left))
                         (if (> i right)
                           ; missed all columns, no hit
                           #f
                           ; check if it hit this column
                           (let* ((column (vector-ref invaders i))
                                  (a-x (column 'x)))
                             (if (and (>= b-x a-x)
                                      (<= b-x (+ a-x invader-width)))
                               ; if it hit the column, get result
                               (let ((hit ((column 'shot!) b-y)))
                                 ; true if the column that got hit
                                 ; was most left or right one
                                 ; and no longer alive
                                 (when (and (not (column 'alive?))
                                            (or (= i left) (= i right)))
                                   (cond
                                     ; column that died was last one
                                     ; so player wins
                                     ((= left right)
                                      (set! victory #t))
                                     ; left column died, so update
                                     ((= i left)
                                      (update-left!))
                                     ; right column died so update
                                     ((= i right)
                                      (update-right!))))
                                 hit)
                               ; didn't hit column, keep looking
                               (loop (+ i 1))))))))
                 ; when there's a kill, increase speed
                 (when (kill? result)
                   (speed!))
                 ; returns the result of the shot and victory state
                 (values result victory)))))

         ;; moves all the columns
         ;; also gets the left and right most active invaders in the swarm
         ;; for bound checking purposes
         (move!
           (let (; when hitting the edge of the screen
                 ; down decides if swarm moves down or changes direction
                 (down #f)
                 ; direction can be left, right, or down
                 (direction 'right))
             (lambda ()
               ; get the bounds of the outer columns
               (let ((left-bound  ((vector-ref invaders left) 'x))
                     (right-bound (+ ((vector-ref invaders right) 'x) invader-width)))
                 ; direction can only change when touching the side of the screen
                 (cond
                   ; when left column touches left side of screen
                   ((<= left-bound 0)
                    (if down
                      (set!-values (down direction) (values #f 'down))
                      (set!-values (down direction) (values #t 'right))))
                   ; when right column touches right side of screen
                   ((>= right-bound 1)
                    (if down
                      (set!-values (down direction) (values #f 'down))
                      (set!-values (down direction) (values #t 'left)))))
                 ; move all columns in current direction
                 (vector-map (lambda (column) ((column 'move!) direction)) invaders)))))

         ;; draws the invaders
         (draw!
           (lambda (window)
             (vector-map (lambda (column) ((column 'draw!) window)) invaders)))

         (dispatch
           (lambda (msg)
             (case msg
               ((x-bounds) (values 0 1))
               ((y-bounds) (values 1 0))
               ((shoot) (shoot))
               ((speed) speed)
               ((shot!) shot!)
               ((move!) move!)
               ((draw!) draw!)
               (else
                 (raise-arguments-error
                   'swarm-adt
                   "invalid argument"
                   "given" msg))))))
    dispatch))

