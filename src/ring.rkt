#lang racket

(provide ring
         build-ring
         ring-head
         ring-for-each
         ring-assoc
         ring-next!
         ring-add!)

(define (ring size (item #f))
  (let ((rng (mcons item '())))
    (do ((i 1 (+ i 1))
         (r rng (mcdr r)))
      ((= i size) (set-mcdr! r rng ) (mcons rng size))
      (set-mcdr! r (mcons item '())))))

(define (build-ring size proc)
  (let ((rng (mcons (proc 0) '())))
    (do ((i 1 (+ i 1))
         (r rng (mcdr r)))
      ((= i size) (set-mcdr! r rng ) (mcons rng size))
      (set-mcdr! r (mcons (proc i) '())))))

(define (ring-head ring-object)
  (mcar (mcar ring-object)))

(define (ring-for-each f ring-object)
  (let ((rng (mcar ring-object))
        (size (mcdr ring-object)))
    (do ((i 0 (+ i 1))
         (r rng (mcdr r)))
      ((= i size))
      (when (mcar r) (f (mcar r))))))

(define (ring-assoc v ring-object)
  (let ((rng (mcar ring-object))
        (size (mcdr ring-object)))
    (let loop ((i 0) (r rng))
      (cond
        ((and (mcar r) (eq? v (car (mcar r)))) (mcar r))
        ((> i size) (error 'a-ffuck))
        (else (loop (+ i 1) (mcdr r)))))))

(define (ring-next! ring-object)
  (set-mcar! ring-object (mcdr (mcar ring-object))))
 
(define (ring-add! ring-object item)
  (ring-next! ring-object)
  (set-mcar! (mcar ring-object) item))

