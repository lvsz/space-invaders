#lang racket

(require "window.rkt")

(provide swarm-adt)

(define aliens/column 5)
(define aliens/row 11)
(define alien-width (* 12 unit-width))
(define alien-height (* 8 unit-height))
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

(define (kill? x)
  (and (integer? x) (positive? x)))

;;; adt for a single alien object
;;; requires an x-coordinate, identifier, and an integer for alien type
(define (alien-adt y make-id type)
  (let*
    ;; identifier to store state needed by the window adt
    ((id (make-id type))

     ;; "health bar"
     ;; varies according to alien type
     (health (+ (quotient type 2) 1))

     ;; points upon killing are based on initial health
     (points (* 100 health))

     ;; is it alive?
     (alive?
       (lambda ()
         (positive? health)))

     ;; this kills the alien
     ;; or damages it if it still has health left
     (hit!
       (lambda ()
         (if (not (alive?))
           #f
           (begin
             (set! health (- health 1))
             (if (alive?) 0 points)))))

     ;; moves the alien on the y-axis
     ;; position on the x-axis is stored in the alien-column adt
     (move!
       (lambda ()
         (set! y (+ y y-move))))
     
     ;; draws and animates the alien
     ;; requires window adt and y coordinate
     (draw!
       (lambda (window x)
         (cond
           ;; if alive, just draw and animate
           ((alive?)
             ((window 'draw!) id x y)
             ((window 'animate!) id))
           ;; if health is 1, remove alien graphic
           ;; then create a new id for the explosion animation
           ;; draws the explosion and sets health to 0
           ((= health 1)
             ((window 'remove!) id)
             (set! id (make-id -1))
             ((window 'draw!) id x y)
             (set! health 0))
           ;; when health is 0, remove explosion graphic
           ;; then change health to -1, for which this conditional returns null
           ((= health 0)
             ((window 'remove!) id)
             (set! health -1)))))

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
               'alien-adt
               "invalid argument"
               "given" msg))))))
    dispatch))


;;; adt that stores a 1 dimensional vector of aliens
;;; requires everything a single alien needs, plus a x coordinate
(define (alien-column x y make-id)
  (let*
    (;; the aliens are stored in a vector
     ;; builds it using the alien-adt from above
     (aliens
       (build-vector
         aliens/column
         (lambda (i)
           (alien-adt (* 2 i alien-height) make-id (- aliens/column i 1)))))

    ;; number of aliens alive, 0 means column is empty
    (alive aliens/column)
     (alive?
       (lambda ()
         (positive? alive)))

     ;; like vector-ref
     (alien-ref
       (lambda (idx)
         (vector-ref aliens idx)))

     ;; top-most active alien in the row
     (top-most  0)

     ;; bottom-most active alien in the row
     ;; default depends on length of row
     (bottom-most (- aliens/column 1))

     ;; updates top-most to new top-most active alien
     (update-top-most!
       (lambda ()
         (let loop ((i (+ top-most 1)))
           (cond (((alien-ref i) 'alive?)
                  (set! top-most i))
                 (else (loop (+ i 1)))))))

     ;; updates bottom-most to new bottom-most active alien
     (update-bottom-most!
       (lambda ()
         (let loop ((i (- bottom-most 1)))
           (cond (((alien-ref i) 'alive?)
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
               (let* ((alien (alien-ref i))
                      (a-y   (alien 'y)))
                  (if (or (< b-y a-y)
                          (> b-y (+ a-y alien-height)))
                    ;; shot missed, continue looking
                    (loop (- i 1))
                    ;; returns the result of a hit
                    ;; an integer if the alien's still alive
                    ;; positive integer if it's also a kill
                    ;; #f if it's already dead
                    (let ((result ((alien 'hit!))))
                      (when (kill? result)
                        (set! alive (- alive 1))
                        (cond
                          ((and (= i bottom-most) (alive?))
                           (update-bottom-most!))
                          ((and (= i top-most) (alive?))
                           (update-top-most!))))
                      result))))))))

     ;; moves the alien column within the window limits
     (move!
       (lambda (direction)
         (when (alive?)
           (if (eq? direction 'down)
             (vector-map (lambda (a) ((a 'move!))) aliens)
             (set! x ((if (eq? direction 'right) + -) x x-move))))))

     ;; maps draw! on all elements
     (draw!
       (lambda (window)
         (vector-map (lambda (a) ((a 'draw!) window x)) aliens)))

     (dispatch
       (lambda (msg)
         (case msg
           ((x) x)
           ((top-bound)  ((alien-ref top-most) 'y))
           ((bottom-bound) (+ ((alien-ref bottom-most) 'y) alien-height))
           ((alive?) (alive?))
           ((shot!)  shot!)
           ((move!)  move!)
           ((draw!)  draw!)
           (else
             (raise-arguments-error
               'alien-column
               "invalid argument"
               "given" msg))))))
    dispatch))


;;; swarm-adt is a row of alien-columns
(define (swarm-adt make-id)
  (let* ((x 0)
         (y 0)

         (game-over #f)

         ;; builds a vector of alien-column datastructures
         (aliens (build-vector
                   aliens/row
                   (lambda (i)
                     ;; calculates spacing between columns
                     (alien-column (+ x (* i alien-width 4/3)) y make-id))))

         ;; left column
         (left 0)
         (update-left!
           (lambda ()
             (let loop ((i (+ left 1)))
               (if ((vector-ref aliens i) 'alive?)
                 (set! left i)
                 (loop (+ i 1))))))

         ;; right column
         (right (- aliens/row 1))
         (update-right!
           (lambda ()
             (let loop ((i (- right 1)))
               (if ((vector-ref aliens i) 'alive?)
                 (set! right i)
                 (loop (- i 1))))))

         (shoot
           (lambda ()
             (let loop ((i (- (random (+ left 1) (+ right 2)) 1)))
               (if ((vector-ref aliens i) 'alive?)
                 (values (+ ((vector-ref aliens i) 'x) (/ alien-width 2))
                         ((vector-ref aliens i) 'bottom-bound))
                 (loop (- (random (+ left 1) (+ right 2)) 1))))))


         ;; intitial speed depends on amount of aliens
         (speed (* aliens/column aliens/row 16))

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
                           #f
                           (let* ((column (vector-ref aliens i))
                                  (a-x (column 'x)))
                             (if (and (>= b-x a-x)
                                      (<= b-x (+ a-x alien-width)))
                               (let ((hit ((column 'shot!) b-y)))
                                 (when (and (kill? hit)
                                            (not (column 'alive?))
                                            (or (= i left) (= i right)))
                                   (cond
                                     ((= left right)
                                      (set! victory #t))
                                     ((= i left)
                                      (update-left!))
                                     ((= i right)
                                      (update-right!))))
                                 hit)
                               (loop (+ i 1))))))))
                 ;; when there's a kill, increase speed
                 (when (kill? result)
                   (speed!))
                 ;; returns the result of the shot and victory state
                 (values result victory)))))

         ;; moves all the columns
         ;; also gets the left and right most active aliens in the swarm
         ;; for bound checking purposes
         (move!
           (let ((down #f)
                 (direction 'right))
             (lambda ()
               (let* ((left-bound  ((vector-ref aliens left) 'x))
                      (right-bound (+ ((vector-ref aliens right) 'x) alien-width)))
                 (cond
                   ((<= left-bound 0)
                    (if down
                      (set!-values (down direction) (values #f 'down))
                      (set!-values (down direction) (values #t 'right))))
                   ((>= right-bound 1)
                    (if down
                      (set!-values (down direction) (values #f 'down))
                      (set!-values (down direction) (values #t 'left)))))
                 (vector-map (lambda (column) ((column 'move!) direction)) aliens)))))
    ;         (let ((min-x ((vector-ref aliens left) 'x))
    ;               (max-x ((vector-ref aliens right) 'x)))
    ;           (vector-map (lambda (column) ((column 'move!) min-x max-x)) aliens))))

         ;; draws the aliens
         (draw!
           (lambda (window)
             (vector-map (lambda (column) ((column 'draw!) window)) aliens)))

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

