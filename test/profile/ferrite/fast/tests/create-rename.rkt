#lang s-exp rosette

(require "../fs.rkt" "../lang.rkt" "../litmus.rkt" "../verifier.rkt" "../synth.rkt" "../ext4.rkt"
         rackunit rackunit/text-ui)

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


; SeqFS
(define (create-rename-allow oldfs newfs)
  (define new-1 (ondisk newfs 1))
  (list (equal? new-1 #f)
        (equal? new-1 '(#t #t))))

; Ext4
(define (create-rename-fs-ext4)
  (ext4-fs #:capacity 2 #:blocksize block-size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define create-rename-synth-tests
  (test-suite
   "create-rename synth test"
   #:before (thunk (printf "-----create-rename synth-----\n"))
   (test-ext4-synth)
   ))

(time (run-tests create-rename-synth-tests))
