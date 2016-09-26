#lang rosette

(require "list.rkt" "../bench.rkt")
(provide (all-defined-out))


; Returns a new list of the form lst[0..pos) ++ lst(pos..).
; Worst-case behavior trigerred when given a symbolic list and position.
(define (remove-at lst pos)
  (bench
   (let-values ([(front back) (split-at lst pos)])
     (append front (rest back)))
   (for/all ([lst lst])
    (let loop ([i 0] [front empty] [back lst])
      (if (= i pos)
          (append front (cdr back))
          (loop (add1 i) (append front (list (car back))) (cdr back)))))))


; Simple test for remove-at
(define (test-remove-at [len 50])
  (define-symbolic* idx integer?)
  (remove-at (symbolic-list len) idx)
  (void))
(profile-bench "slow remove-at" (with-variant 0 (test-remove-at)))
(profile-bench "fast remove-at" (with-variant 1 (test-remove-at)))
