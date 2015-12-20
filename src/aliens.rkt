#lang racket

(require "window.rkt")

(provide swarm-adt)

(define aliens/column 5)
(define aliens/row 11)
(define alien-width (* 12 unit-width))
(define alien-height (* 8 unit-height))

(define (vector-fold proc x . vec)
  (let ((end (apply min (map vector-length vec))))
    (let loop ((i 0) (acc x))
      (if (= i end)
        acc
        (loop (+ i 1)
              (apply proc acc (map (lambda (v) (vector-ref v i)) vec)))))))

(define (alien-adt x make-id type)
  (let* ((id (make-id type))
         (health (quotient (+ type 1) 2))
         (points (* 100 health))
         (alive? #t)
         (kill!
           (lambda ()
             (if (> health 1)
               (begin (set! health (- health 1))
                      0)
               (begin (set! alive? #f)
                      points))))
         (move!
           (let ((x-diff (* 2 unit-width)))
             (lambda (direction)
               (case direction
                 ((left)  (set! x (- x x-diff)))
                 ((right) (set! x (+ x x-diff)))))))
         (draw!
           (lambda (window y)
             (cond (alive?
                    ((window 'draw!) id x y)
                    ((window 'animate!) id))
                   ((= health 1)
                    ((window 'draw!) id 1 y)
                    (set! id (make-id 0))
                    ((window 'draw!) id x y)
                    (set! health 0))
                   ((= health 0)
                    ((window 'draw!) id 1 y)
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

(define (alien-row x y make-id type)
  (let* ((alive? #t)
         (aliens
           (build-vector
             aliens/row
             (lambda (i)
               (alien-adt (+ x (* 4/3 i alien-width)) make-id type))))
         (alien-ref
           (lambda (idx)
             (vector-ref aliens idx)))
         (leftmost  0)
         (rightmost (- aliens/row 1))
         (set-leftmost!
           (lambda ()
             (let loop ((i (+ leftmost 1)))
               (cond (((alien-ref i) 'alive?)
                      (set! leftmost i))
                     (else (loop (+ i 1)))))))
         (set-rightmost!
           (lambda ()
             (let loop ((i (- rightmost 1)))
               (cond (((alien-ref i) 'alive?)
                      (set! rightmost i))
                     (else (loop (- i 1)))))))
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
                          (let ((score ((aln 'kill!))))
                            (cond ((zero? score) (void))
                                  ((= leftmost rightmost) (set! alive? #f))
                                  ((= idx leftmost)  (set-leftmost!))
                                  ((= idx rightmost) (set-rightmost!)))
                            score))
                         (else #f)))))))
         (move!
           (let ((direction 'right))
             (lambda (min-x max-x)
               (when alive?
                 (cond
                   ((and (<= min-x 0)
                         (eq? direction 'left))
                    (set! direction 'right)
                    (set! y (+ y alien-height)))
                   ((and (>= max-x 1)
                         (eq? direction 'right))
                    (set! direction 'left)
                    (set! y (+ y alien-height)))
                   (else
                     (vector-map (lambda (a)
                                   ((a 'move!) direction)) aliens)))))))
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

(define (swarm-adt make-id)
  (let* ((x 0)
         (y 0)
         (aliens (build-vector
                   aliens/column
                   (lambda (i)
                     (alien-row x (+ (* -2 alien-height i)
                                     (* 10 alien-height)) make-id (+ i 1)))))
         (top (- aliens/column 1))
         (bottom 0)
         (speed (* aliens/column aliens/row 16))
         (speed!
           (lambda ()
             (set! speed (- speed 8))))
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

