#lang rosette

(require "lang.rkt")

; Requires a mix instruction
(time
 (synthesize-program
  (lambda (grid)
    (begin
      (grid-set! grid (point 0 0) 'a)
      (grid-set! grid (point 1 2) 'b)))
  (lambda (grid) (and (equal? 'c (grid-ref grid (point 3 3)))))))
