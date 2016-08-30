#lang rosette

(require "hide.rkt" (only-in rosette/lib/profile profile-thunk))



(define (append-if xs ys)
   (if (= (length xs) (length ys))
       (hide xs ys)
       (hide xs xs)))

(define-symbolic b boolean?)

(profile-thunk (thunk (append-if (if b null '(1 2 3 4)) '(4 3 2 1))))