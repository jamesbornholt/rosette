#lang rosette

(require "hide.rkt" "../bench.rkt" (only-in rosette/lib/profile profile-thunk))



(define (append-if xs ys)
  (bench
   (if (= (length xs) (length ys))
       (hide xs ys)
       (hide xs xs))
    (for*/all ([xs xs][ys ys])  
     (if (= (length xs) (length ys))
         (list-set xs ys)
         (list-set xs xs)))))

(define-symbolic b boolean?)

(profile-thunk (thunk (parameterize ([variant 0]) (append-if (if b null '(1 2 3 4)) '(4 3 2 1)))))