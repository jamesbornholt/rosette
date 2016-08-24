#lang rosette

(require "lib/fs.rkt" "lib/lang.rkt" "lib/litmus.rkt" 
         "lib/verifier.rkt" "lib/synth.rkt" "lib/ext4.rkt"
         "../bench.rkt"
         rosette/lib/roseunit rackunit rackunit/text-ui)

(define small? #f)
(define block-size (if small? 64 4096))

(define create-rename-setup '())

(define create-rename-test-incorrect
  (list
   (creat 0)
   (write 0 '(#t #t))
   ; (fsync 0)
   (rename 0 1)))

(define create-rename-test-correct
  (list
   (creat 0)
   (write 0 '(#t #t))
   (fsync 0)
   (rename 0 1)))

(define (create-rename-allow oldfs newfs)
  (define new-1 (ondisk newfs 1))
  (list (equal? new-1 #f)
        (equal? new-1 '(#t #t))))

(define (create-rename-fs-ext4)
  (ext4-fs #:capacity 2 #:blocksize block-size))


(define (test-ext4-synth)
  (define test
    (litmus create-rename-fs-ext4 create-rename-setup create-rename-test-incorrect create-rename-allow))
  (define prog (synth test))
  (check-false (false? prog))
  (check-false (term? prog))
  (printf "create-rename ext4:\n  before:  ~v\n  after: ~v\n"
          create-rename-test-incorrect
          (remove-disabled-syncs prog))
  (define cost (sync-cost prog))
  (check equal? cost 1)
  (define test*
    (litmus create-rename-fs-ext4 create-rename-setup prog create-rename-allow))
  (define-values (cex state) (verify-correctness test*))
  (check-true (unsat? cex)))


(define rename-tests:slow
  (test-suite+
   "Create-rename ext4 tests for slow version of Ferrite"
   (parameterize ([variant 0][merge-structs? #t])
     (test-ext4-synth))))
(define rename-tests:fast
  (test-suite+
   "Create-rename ext4 tests for fast version of Ferrite"
   (parameterize ([variant 1][merge-structs? #f])
     (test-ext4-synth))))


(time (run-tests rename-tests:slow))
(time (run-tests rename-tests:fast))
