#lang racket

(require (except-in "game.rkt" game))

;;; opens 6 space invaders windows with random input
(define games
  (build-list 6 (lambda (x)
                  (game-init (string (integer->char (+ 65 x))) #t))))

(for-each (lambda (game)
            (game 'start)) games)

