#lang rosette

(require (only-in rosette/lib/profile profile-thunk))

(define-syntax-rule (hide xs ys) (append xs ys))

(define (append-if xs ys)
   (if (= (length xs) (length ys))
       (hide xs ys)
       (hide xs xs)))

(profile-thunk (thunk (append-if '(1 2 3 4) '(4 3 2 1))))