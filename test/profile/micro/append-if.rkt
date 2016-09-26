#lang rosette

(require "list.rkt" "../bench.rkt")
(provide (all-defined-out))


; Returns xs ++ ys if xs and ys have the same length;
; othwerwise returns xs ++ xs.  Worst-case behavior
; trigerred when given two symbolic lists.
(define (append-if xs ys)
  (bench
   (if (= (length xs) (length ys))
       (append xs ys)
       (append xs xs))
   (for*/all ([xs xs][ys ys])  
     (if (= (length xs) (length ys))
         (append xs ys)
         (append xs xs)))))


; Simple test for append-if
(define (test-append-if left right)
  (append-if left right)
  (void))

(define len 50)
(define lstA (symbolic-list len))
(define lstB (symbolic-list len))

(profile-bench! "slow append-if" (with-variant 0 (test-append-if lstA lstB)))
(profile-bench! "fast append-if" (with-variant 1 (test-append-if lstA lstB)))
