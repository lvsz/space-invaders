#lang racket

(require "window.rkt")
(provide bullets-adt
         bullet-width
	     bullet-height)


(define bullet-width  (*  1 unit-width))
(define bullet-height (*  4 unit-height))

;;; bullet adt
;;; type can either be player or invader
;;; also requires coordinates and id gemerator
;;; window is needed to remove the bullet's image after it hit something
(define (bullet-adt type x y make-id window)
  (let*
    ((id (make-id))

     ;; when true, the bullet moves and gets drawn
     ;; when false, the data structure it's located in can safely delete it
     (active #t)

     ;; makes it inactive and removes it from the window
     (explode!
       (lambda ()
         (set! active #f)
         ((window 'remove!) id)))

     (move
       (if (eq? type 'player)
         (lambda (y)
           (- y (* 2 bullet-height)))
         (lambda (y)
           (+ y (* 3/2 bullet-height)))))

     ;; moves the bullet when active
     (move!
       (lambda ()
         (when active
           (set! y (move y))
           ; inactivates the bullet upon hitting the top border
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
           ((active?)   active)
           ((explode!)  explode!)
           ((move!)     move!)
           ((draw!)     draw!)
           (else
             (raise-arguments-error
               'bullet-adt
               "invalid argument"
               "given" msg))))))
    dispatch))


;;; some helper functions for the bullets-adt, which relies on mutable lists
;;; may get moved to a seperate module because they're generic

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


;;; the bullets adt is a data structure that holds and modifies every bullet
;;; the make-id function is needed to generate new bullets
;;; window is needed to clear their graphics
(define (bullets-adt make-id window)
  (let*
    (;; starts by making a headed mutable list
     (bullets (mcons 'bullets '()))

     ;; a for-each function that only needs a procedure
     (bullet-for-each
       (lambda (proc)
         (mfor-each proc (mcdr bullets))))

     ;; shoot! creates a new bullet and adds it to the structure
     ;; type can be either player or invader
     (shoot!
       (lambda (type x y)
         (mappend! bullets (bullet-adt type x y make-id window))))

     ;; goes through all bullets till it finds an active one
     ;; the ones before get removed from the list
     (clean-up!
       (lambda ()
         (let loop ((active-bullets (mcdr bullets)))
           (cond
             ((null? active-bullets)
              (set-mcdr! bullets '()))
             (((mcar active-bullets) 'active?)
              (set-mcdr! bullets active-bullets))
             (else
              (loop (mcdr active-bullets)))))))

     ;; move! first tries to clean up any inactive bullets
     ;; then calls move! on the remaining one
     (move!
       (lambda ()
         ; make sure there are any bullets to move
         (when (mpair? (mcdr bullets))
           (clean-up!)
           (bullet-for-each (lambda (b) ((b 'move!)))))))

     ;; draws! all bullets
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
