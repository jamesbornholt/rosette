#lang rosette

; Micro-benchmarks for various list operations.

(require "../bench.rkt" rosette/lib/angelic
         rosette/lib/profile rosette/lib/profile/renderer/html)
(provide (all-defined-out))


; Construct a symbolic list of up to the given length
(define (symbolic-list len)
  (define lst (build-list len identity))
  (apply choose* (for/list ([i len]) (take lst i))))

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

; Like lst-set, returns a new list that updates lst[pos] with val.
; Unlike lst-set, this shows the difference between having multiple
; and a single recursive call. Worst-case behavior trigerred when
; given a concrete list and a symbolic position.  In particular,
; both the slow and fast version produce a concrete list of size N,
; but the symbolic value's at the ith position are of size O(i) in
; the slow case and O(1) in the fast case.
; > (define-symbolic i integer?)
; > (update '(1 2 3 4 5) i -1)
(define (update lst pos val)
  (bench
   (match lst
     [(list) lst]
     [(list x xs ...)
      (if (= pos 0) 
          (cons val xs)
          (cons x (update xs (- pos 1) val)))])
   (match lst
     [(list) lst]
     [(list x xs ...)
      (cons (if (= pos 0) val x) (update xs (- pos 1) val))])))



; Simple test for list-set
(define (test-list-set [len 50])
  (define-symbolic* idx val integer?)
  (list-set (symbolic-list len) idx -1)
  (void))
(profile-bench "slow list-set" (with-variant 0 (test-list-set)))
(profile-bench "fast list-set" (with-variant 1 (test-list-set)))


; Simple test for remove-at
(define (test-remove-at [len 50])
  (define-symbolic* idx integer?)
  (remove-at (symbolic-list len) idx)
  (void))
(profile-bench "slow remove-at" (with-variant 0 (test-remove-at)))
(profile-bench "fast remove-at" (with-variant 1 (test-remove-at)))


; Simple test for append-if
(define (test-append-if [len 50])
  (append-if (symbolic-list len) (symbolic-list len))
  (void))
(profile-bench "slow append-if" (with-variant 0 (test-append-if)))
(profile-bench "fast append-if" (with-variant 1 (test-append-if)))


; Simple test for update
(define (test-update [len 50])
  (define-symbolic* idx integer?)
  (update (build-list len identity) idx -1)
  (void))
(profile-bench "slow update" (with-variant 0 (test-update)))
(profile-bench "fast update" (with-variant 1 (test-update)))
