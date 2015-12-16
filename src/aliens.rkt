#lang racket

(require "window.rkt")

(provide swarm-adt)

(define aliens/column 5)
(define aliens/row 11)
(define alien-width (* 12 unit-width))
(define alien-height (* 8 unit-height))

(define (vector-fold proc x vec)
  (let loop ((i 0) (acc x))
    (if (= i (vector-length vec))
      acc
      (loop (+ i 1) (proc acc (vector-ref vec i))))))

(define (alien-adt x y id)
  (let* ((alive? #t)
         (set-y!
           (lambda (new-y)
             (set! y new-y)))
         (kill!
           (lambda ()
             (set! alive? #f)
             100))
         (move!
           (let ((x-diff (* 2 unit-width)))
             (lambda (direction)
               (case direction
                 ((left)  (set! x (- x x-diff)))
                 ((right) (set! x (+ x x-diff)))
                 ((down)  (set! y (+ y alien-height)))))))
         (draw!
           (lambda (window)
             (when alive?
             ((window 'draw!) id x y)
             ((window 'animate!) id))))
         (dispatch
           (lambda (msg)
             (case msg
               ((x) x)
               ((y) y)
               ((alive?) alive?)
               ((set-y!) set-y!)
               ((kill!)  kill!)
               ((move!)  move!)
               ((draw!)  draw!)
               (else
                 (raise-arguments-error
                   'alien-adt
                   "invalid argument"
                   "given" msg))))))
    dispatch))

(define (alien-row type x y make-id)
  (let* ((alive? #t)
         (aliens
           (build-vector
             aliens/row
             (lambda (i)
               (alien-adt (+ x (* 4/3 i alien-width)) y (make-id type)))))
         (alien-ref
           (lambda (idx)
             (vector-ref aliens idx)))
         (leftmost  0)
         (rightmost (- aliens/row 1))
         (set-leftmost!
           (lambda ()
             (let loop ((i (+ leftmost 1)))
               (cond ((> i rightmost)
                      (set! alive? #f))
                     (((alien-ref i) 'alive?)
                      (set! leftmost i))
                     (else (loop (+ i 1)))))))
         (set-rightmost!
           (lambda ()
             (let loop ((i (- rightmost 1)))
               (cond (((alien-ref i) 'alive?)
                      (set! rightmost i))
                     ((< i leftmost)
                      (set! alive? #f))
                     (else (loop (- i 1)))))))
         (shot!
           (lambda (b-x)
             (let bsearch ((i leftmost) (j rightmost))
               (if (> i j)
                 #f
                 (let* ((mid  (quotient (+ i j) 2))
                        (aln  (alien-ref mid))
                        (a-x1 (aln 'x))
                        (a-x2 (+ a-x1 alien-width)))
                   (cond ((< b-x a-x1)
                          (bsearch i (- mid 1)))
                         ((> b-x a-x2)
                          (bsearch (+ mid 1) j))
                         ((aln 'alive?)
                          (cond ((= mid leftmost)  (set-leftmost!))
                                ((= mid rightmost) (set-rightmost!)))
                          ((aln 'kill!)))
                         (else #f)))))))
         (move!
           (let ((direction 'right))
             (lambda (min-x max-x)
               (cond
                 ((and (<= min-x 0)
                       (eq? direction 'left))
                  (set! direction 'right)
                  (set! y (+ y alien-height))
                  (vector-map (lambda (a) ((a 'set-y!) y)) aliens))
                 ((and (>= max-x 1)
                       (eq? direction 'right))
                  (set! direction 'left)
                  (set! y (+ y alien-height))
                  (vector-map (lambda (a) ((a 'set-y!) y)) aliens))
                 (else
                  (vector-map (lambda (a) ((a 'move!) direction)) aliens))))))
         (draw!
           (lambda (window)
             (vector-map (lambda (a) ((a 'draw!) window)) aliens)))
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

(define (swarm-adt make-id)
  (let* ((x 0)
         (y 0)
         (aliens (vector (alien-row 1 x (* 9 alien-height) make-id)
                         (alien-row 1 x (* 7 alien-height) make-id)
                         (alien-row 2 x (* 5 alien-height) make-id)
                         (alien-row 2 x (* 3 alien-height) make-id)
                         (alien-row 3 x (* 1 alien-height) make-id)))
         (top (- aliens/column 1))
         (bottom 0)
         (speed (* aliens/column aliens/row 16))
         (speed!
           (lambda ()
             (set! speed (- speed 16))))
      ;   (x-bounds
      ;     (lambda ()
      ;       (unless ((vector-ref aliens bottom) 'alive?)
      ;         (do ((i (+ bottom 1) (+ i 1)))
      ;           (((vector-ref aliens i) 'alive?) (set! bottom i))))
      ;       (unless ((vector-ref aliens top) 'alive?)
      ;         (do ((i (- top 1) (- i 1)))
      ;           (((vector-ref aliens i) 'alive?) (set! top i))))
      ;       (cons (+ ((vector-ref aliens top) 'x) alien-height)
      ;             (+ ((vector-ref aliens bottom 'x) alien-height)))))
      ;   (y-bounds
      ;     (lambda ()
      ;     (let loop ((i bottom) (left 1) (right 0))
      ;       (let ((left-bound 0); ((vector-ref aliens i) 'left-bound))
      ;             (right-bound 10)); ((vector-ref aliens i) 'right-bound)))
      ;         (if (= i top)
      ;           (cons (min left left-bound)
      ;                 (+ (max right right-bound) alien-width))
      ;           (loop (+ i 1) (min left left-bound) (max right right-bound)))))))
         (shot!
           (lambda (b-x b-y)
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
               (when hit?
                 (speed!))
               hit?)))
         (move!
           (lambda ()
             (let ((min-x (vector-fold (lambda (acc x)
                                         (min acc (x 'left-bound)))  1 aliens))
                   (max-x (vector-fold (lambda (acc x)
                                         (max acc (x 'right-bound))) 0 aliens)))
               (vector-map (lambda (row) ((row 'move!) min-x max-x)) aliens))))
         (draw!
           (lambda (window)
             (vector-map (lambda (row) ((row 'draw!) window)) aliens)))
         (dispatch
           (lambda (msg)
             (case msg
               ((x-bounds) (values 0 1)) ;(x-bounds))
               ((y-bounds) (values 1 0)) ;(y-bounds))
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

