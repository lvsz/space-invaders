#lang racket

(require (except-in racket/base for-each assoc))

(provide new for-each assoc add!)

(define (new size)
  (let ((ring (mcons #f '())))
    (do ((i 1 (+ i 1))
         (r ring (mcdr r)))
      ((= i  size) (set-mcdr! r ring) (mcons ring size))
      (set-mcdr! r (mcons #f '())))))

(define (for-each f ring-object)
  (let ((ring (mcar ring-object))
        (size (mcdr ring-object)))
    (do ((i 0 (+ i 1))
         (r ring (mcdr r)))
      ((= i size))
      (when (mcar r) (f (mcar r))))))

(define (assoc v ring-object)
  (let ((ring (mcar ring-object))
        (size (mcdr ring-object)))
    (let loop ((i 0) (r ring))
      (cond
        ((and (mcar r) (eq? v (car (mcar r)))) (mcar r))
        ((> i size) (error 'a-ffuck))
        (else (loop (+ i 1) (mcdr r)))))))

(define-syntax add!
  (syntax-rules ()
    ((add! ring-object item)
     (begin
       (set! ring-object (mcons (mcdr (mcar ring-object)) (mcdr ring-object)))
       (set-mcar! (mcar ring-object) item)))))
