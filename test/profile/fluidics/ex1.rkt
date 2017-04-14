#lang rosette

(require "lang.rkt")

(time
 (synthesize-program
  (lambda (grid) (grid-set! grid (point 0 0) 'a))
  (lambda (grid) (equal? 'a (grid-ref grid (point 4 2))))))
