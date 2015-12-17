#lang racket

(require (except-in "game.rkt" game))

(define games
  (build-list 6 (lambda (x)
                  (game-init (string (integer->char (+ 65 x))) #t))))

(for-each (lambda (game)
            (game 'start)) games)

