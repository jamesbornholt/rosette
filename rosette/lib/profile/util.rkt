#lang racket

(provide (all-defined-out))

(define (cons-box-atomic! box car)
  (let* ([old (unbox box)]
         [new (cons car old)])
    (let loop ()
      (unless (box-cas! box old new)
        (loop)))))