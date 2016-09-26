#lang rosette

(require "list.rkt" "../bench.rkt")
(provide (all-defined-out))


; Returns a new list that updates lst[pos] with val.
; Worst-case behavior trigerred when given a symbolic list and position.
(define (list-set lst pos val)
  (bench
   (let-values ([(front back) (split-at lst pos)])
     (append front (cons val (cdr back))))
   (for/all ([lst lst]) 
    (map (lambda (i v) (if (= pos i) val v))
         (build-list (length lst) identity)
         lst))))


; Simple test for list-set
(define (test-list-set [len 50])
  (define-symbolic* idx val integer?)
  (list-set (symbolic-list len) idx -1)
  (void))
(profile-bench "slow list-set" (with-variant 0 (test-list-set)))
(profile-bench "fast list-set" (with-variant 1 (test-list-set)))
