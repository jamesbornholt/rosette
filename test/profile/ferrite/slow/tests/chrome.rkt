#lang s-exp rosette

(require "../fs.rkt" "../lang.rkt" "../litmus.rkt" "../verifier.rkt" "../synth.rkt" "../ext4.rkt"
         rackunit rackunit/text-ui)

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

; SeqFS
(define (chrome-allow oldfs newfs)
  ; file must be a prefix of #ts
  (define new-0 (ondisk newfs 0))
  (list (apply && new-0)))

; Ext4
(define (chrome-fs-ext4-nodelalloc)
  (ext4-fs #:capacity 2 #:blocksize block-size #:nodelalloc? #t))

(define (test-ext4-synth-nodelalloc)
  (printf "test-ext4-synth-nodelalloc ~a\n" small?)
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

(define chrome-synth-tests
  (test-suite
   "chrome synth test"
   (test-ext4-synth-nodelalloc)))

(time (run-tests chrome-synth-tests))
