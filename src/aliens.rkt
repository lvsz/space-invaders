#lang racket

(require "window.rkt")

(provide swarm-adt)

(define aliens/column 5)
(define aliens/row 11)
(define alien-width (* 12 unit-width))
(define alien-height (* 8 unit-height))

;; generic fold function for vectors
;; ought to work exactly like fold-left in R6RS
(define (vector-fold proc x . vec)
  (let ((end (apply min (map vector-length vec))))
    (let loop ((i 0) (acc x))
      (if (= i end)
        acc
        (loop (+ i 1)
              (apply proc acc (map (lambda (v) (vector-ref v i)) vec)))))))

;;; adt for a single alien object
;;; requires an x-coordinate, identifier, and an integer for alien type
(define (alien-adt x make-id type)
  (let*
    ;; identifier to store state needed by the window adt
    ((id (make-id type))

     ;; "health bar"
     ;; varies according to alien type
     (health (+ (quotient type 2) 1))

     ;; points upon killing are based on initial health
     (points (* 100 health))

     ;; is it alive?
     (alive? #t)

     ;; this kills the alien
     ;; or damages it if it still has health left
     (kill!
       (lambda ()
         (if (> health 1)
           ;; when there's still health left
           ;; subtract 1, and return a score of 0 points
           (begin (set! health (- health 1))
                  0)
           ;; otherwise, set alive? to false and return the score value
           (begin (set! alive? #f)
                  points))))

     ;; moves the alien on the x-axis
     ;; position on the y-axis is stored in the alien-row adt
     (move!
       (let ((x-diff (* 2 unit-width)))
         (lambda (direction)
           (case direction
             ((left)  (set! x (- x x-diff)))
             ((right) (set! x (+ x x-diff)))))))
     
     ;; draws and animates the alien
     ;; requires window adt and y coordinate
     (draw!
       (lambda (window y)
         (cond
           ;; if alive, just draw and animate
           (alive?
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
           ((x) x)
           ((alive?) alive?)
           ((kill!)  kill!)
           ((move!)  move!)
           ((draw!)  draw!)
           (else
             (raise-arguments-error
               'alien-adt
               "invalid argument"
               "given" msg))))))
    dispatch))


;;; adt that stores a 1 dimensional vector of aliens
;;; requires everything a single alien needs, plus a y coordinate
(define (alien-row x y make-id type)
  (let*
    ((alive? #t)

     ;; the aliens are stored in a vector
     ;; builds it using the alien-adt from above
     (aliens
       (build-vector
         aliens/row
         (lambda (i)
           (alien-adt (+ x (* 4/3 i alien-width)) make-id type))))

     ;; like vector-ref
     (alien-ref
       (lambda (idx)
         (vector-ref aliens idx)))

     ;; leftmost active alien in the row
     (leftmost  0)

     ;; rightmost active alien in the row
     ;; default depends on length of row
     (rightmost (- aliens/row 1))

     ;; updates leftmost to new leftmost active alien
     (set-leftmost!
       (lambda ()
         (let loop ((i (+ leftmost 1)))
           (cond (((alien-ref i) 'alive?)
                  (set! leftmost i))
                 (else (loop (+ i 1)))))))

     ;; updates rightmost to new rightmost active alien
     (set-rightmost!
       (lambda ()
         (let loop ((i (- rightmost 1)))
           (cond (((alien-ref i) 'alive?)
                  (set! rightmost i))
                 (else (loop (- i 1)))))))

     ;; incoming shot
     ;; uses binary search to see if it hit an active alien in the vector
     (shot!
       (lambda (b-x)
         (let bsearch ((i leftmost) (j rightmost))
           (if (> i j)
             #f
             (let* ((idx  (quotient (+ i j) 2))
                    (aln  (alien-ref idx))
                    (a-x1 (aln 'x))
                    (a-x2 (+ a-x1 alien-width)))
               (cond ((< b-x a-x1)
                      (bsearch i (- idx 1)))
                     ((> b-x a-x2)
                      (bsearch (+ idx 1) j))
                     ((aln 'alive?)
                      ;; check score value to determine outcome
                      (let ((score ((aln 'kill!))))
                        (cond
                          ;; zero means no kill
                          ((zero? score) (void))
                          ;; if true, every alien in this row is dead
                          ;; so set alive? to false
                          ((= leftmost rightmost) (set! alive? #f))
                          ;; if leftmost alien was killed, set new leftmost
                          ((= idx leftmost)  (set-leftmost!))
                          ;; if rightmost alien was killed, set new rightmost
                          ((= idx rightmost) (set-rightmost!)))
                        ;; returns score the game will add
                        score))
                     ;; no hits, so return false
                     (else #f)))))))
     
     ;; moves the alien row within the window limits
     (move!
       ;; the swarm starts out moving to the right
       (let ((direction 'right))
         ;; min-x and max-x are the left and right most aliens of the swarm
         ;; these are needed to stay in formation after taking losses
         (lambda (min-x max-x)
           (when alive?
             (cond
               ;; when swarm is going left and touches left border
               ;; go down a step, and switch direction to right
               ((and (<= min-x 0)
                     (eq? direction 'left))
                (set! y (+ y alien-height))
                (set! direction 'right))
               ;; when going right, and touching right border
               ;; go down a step, and switch direction to left
               ((and (>= max-x 1)
                     (eq? direction 'right))
                (set! y (+ y alien-height))
                (set! direction 'left))
               ;; else just move all aliens
               (else
                 (vector-map (lambda (a)
                               ((a 'move!) direction)) aliens)))))))

     ;; maps draw! on all elements
     (draw!
       (lambda (window)
         (vector-map (lambda (a) ((a 'draw!) window y)) aliens)))

     (dispatch
       (lambda (msg)
         (case msg
           ((y) y)
           ((left-bound)  ((alien-ref leftmost) 'x))
           ((right-bound) (+ ((alien-ref rightmost) 'x) alien-width))
           ((alive?) alive?)
           ((shot!)  shot!)
           ((move!)  move!)
           ((draw!)  draw!)
           (else
             (raise-arguments-error
               'alien-row
               "invalid argument"
               "given" msg))))))
    dispatch))


;;; swarm-adt is a stack of alian-rows
(define (swarm-adt make-id)
  (let* ((x 0)
         (y 0)

         ;; builds a vector of alien-row datastructures
         (aliens (build-vector
                   aliens/column
                   (lambda (i)
                     ;; calculates spacing between rows
                     ;; and the type of alien they'll carry
                     (alien-row x (+ (* -2 alien-height i)
                                     (* 10 alien-height)) make-id i))))

         ;; topmost row
         (top (- aliens/column 1))

         ;; bottom row
         (bottom 0)

         ;; intitial speed depends on amount of aliens
         (speed (* aliens/column aliens/row 16))

         ;; changes speed
         (speed!
           (lambda ()
             (set! speed (- speed 8))))

         ;; checks from bottom to top if any bullet can hit something
         (shot!
           (lambda (b-x b-y)
             ;; gets the score value, if any
             ;; no hit is false
             ;; a hit returns an integer
             (let ((hit?
                     (let loop ((i bottom))
                       (if (> i top)
                         #f
                         (let* ((a-row (vector-ref aliens i))
                                (a-y (a-row 'y)))
                           (if (or (< b-y a-y)
                                   (> b-y (+ a-y alien-height)))
                             (loop (+ i 1))
                             ((a-row 'shot!) b-x)))))))
               ;; when there's a hit, increase speed
               (when hit?
                 (speed!))
               ;; return the score value
               hit?)))

         ;; moves all the rows
         ;; also gets the left and right most active aliens in the swarm
         ;; for bound checking purposes
         (move!
           (lambda ()
             (let ((min-x (vector-fold (lambda (acc x)
                                         (if (x 'alive?)
                                           (min acc (x 'left-bound))
                                           acc))
                                         1 aliens))
                   (max-x (vector-fold (lambda (acc x)
                                         (if (x 'alive?)
                                           (max acc (x 'right-bound))
                                           acc))
                                         0 aliens)))
               (vector-map (lambda (row) ((row 'move!) min-x max-x)) aliens))))

         ;; draws the aliens
         (draw!
           (lambda (window)
             (vector-map (lambda (row) ((row 'draw!) window)) aliens)))

         (dispatch
           (lambda (msg)
             (case msg
               ((x-bounds) (values 0 1))
               ((y-bounds) (values 1 0))
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

