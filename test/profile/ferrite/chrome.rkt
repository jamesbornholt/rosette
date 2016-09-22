#lang s-exp rosette

(require "lib/fs.rkt" "lib/lang.rkt" "lib/litmus.rkt" 
         "lib/verifier.rkt" "lib/synth.rkt" "lib/ext4.rkt"
         "../bench.rkt"
         rosette/lib/roseunit rackunit rackunit/text-ui
         rosette/lib/profile rosette/lib/profile/renderer/html)

(define small? #t)
(define writes (if small? '(33 2 31) '(2509 13 2500)))
(define block-size (if small? 64 4096))

(define chrome-setup 
  (list
   (creat 0)  ; fd 0
   (write 0 (for/list ([i (first writes)]) #t))
   (fsync 0)))
(define chrome-test
  (list
   (write 0 (for/list ([i (second writes)]) #t))
   (write 0 (for/list ([i (third writes)]) #t))))
(define (chrome-allow oldfs newfs)
  ; file must be a prefix of #ts
  (define new-0 (ondisk newfs 0))
  (list (apply && new-0)))
(define (chrome-fs-ext4-nodelalloc)
  (ext4-fs #:capacity 2 #:blocksize block-size #:nodelalloc? #t))


(define (test-ext4-synth-nodelalloc)
  (printf "test-ext4-synth-nodelalloc (small=~a)\n" small?)
  (printf "  synth\n")
  (define test
    (litmus chrome-fs-ext4-nodelalloc chrome-setup chrome-test chrome-allow))
  (define prog (synth test))
  (check-false (false? prog))
  (check-false (term? prog))
  (define cost (sync-cost prog))
  (check equal? cost 0)  ; program is already correct with nodelalloc; no fences needed
  (printf "  verify-correctness\n")
  (define test*
    (litmus chrome-fs-ext4-nodelalloc chrome-setup prog chrome-allow))
  (define-values (cex state) (verify-correctness test*))
  (check-true (unsat? cex)))


(define chrome-tests:slow
  (test-suite+
   "Chrome ext4 tests for slow version of Ferrite"
   (parameterize ([variant 0][merge-structs? #t])
     (test-ext4-synth-nodelalloc))))
(define chrome-tests:fast
  (test-suite+
   "Chrome ext4 tests for fast version of Ferrite"
   (parameterize ([variant 1][merge-structs? #f])
     (test-ext4-synth-nodelalloc))))


(profile
  (time (run-tests chrome-tests:slow))
  #:renderer (html-renderer))

(profile
  (time (run-tests chrome-tests:fast))
  #:renderer (html-renderer))
