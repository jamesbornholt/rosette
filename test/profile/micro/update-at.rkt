#lang rosette

(require "list.rkt" "../bench.rkt")
(provide (all-defined-out))


; Like lst-set, returns a new list that updates lst[pos] with val.
; Unlike lst-set, this shows the difference between having multiple
; and a single recursive call. Worst-case behavior trigerred when
; given a concrete list and a symbolic position.  In particular,
; both the slow and fast version produce a concrete list of size N,
; but the symbolic value's at the ith position are of size O(i) in
; the slow case and O(1) in the fast case.
; > (define-symbolic i integer?)
; > (update '(1 2 3 4 5) i -1)
(define (update-at lst pos val)
  (bench
   (match lst
     [(list) lst]
     [(list x xs ...)
      (if (= pos 0) 
          (cons val xs)
          (cons x (update-at xs (- pos 1) val)))])
   (match lst
     [(list) lst]
     [(list x xs ...)
      (cons (if (= pos 0) val x) (update-at xs (- pos 1) val))])))


; Simple test for update-at
(define (test-update-at lst)
  (define-symbolic* idx integer?)
  (update-at lst idx -1)
  (void))

(define lst (build-list 50 identity))

(profile-bench! "slow update-at" (with-variant 0 (test-update-at lst)))
(profile-bench! "fast update-at" (with-variant 1 (test-update-at lst)))
