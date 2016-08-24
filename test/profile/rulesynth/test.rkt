#lang rosette

(require "actions.rkt" "../bench.rkt" "../bench/basic.rkt"
         rosette/lib/angelic rosette/lib/roseunit
         rackunit rackunit/text-ui)
  

(define (test-examples [depth 2] [len 2])
  (test-sat "Easy co test"
   (synth (sketch '((1) (2)) '(+) depth len) co-ex))
  (test-sat  "Easy clt test"
   (synth (sketch '((2) (1 1) (2 1)) '(+ *) depth len) clt-ex))
  (test-sat "Hard mf test"
   (synth  (sketch '((2) (1 1) (1 1 1) (1 1 2 1)) '(+ * =) depth len) mf-ex)))

(define (symbolic-list len)
  (define lst (build-list len identity))
  (apply choose* (for/list ([i len]) (take lst i))))

(define (test-list-set [len 30])
  (define-symbolic* idx val integer?)
  (list-set (symbolic-list len) idx val)
  (void))

(define (test-remove-at [len 30])
  (define-symbolic* idx integer?)
  (remove-at (symbolic-list len) idx)
  (void))

(define example-tests:slow
  (test-suite+
   "Example tests for slow version of actions.rkt"
   (parameterize ([variant 0])
     (test-examples))))

(define example-tests:fast
  (test-suite+
   "Example tests for fast version of actions.rkt"
   (parameterize ([variant 1])
     (test-examples))))

(define list-set-tests:slow
  (test-suite+
   "Artificial list-set tests for slow version of actions.rkt"
   (parameterize ([variant 0])
     (test-list-set))))

(define list-set-tests:fast
  (test-suite+
   "Artificial list-set tests for fast version of actions.rkt"
   (parameterize ([variant 1])
     (test-list-set))))

(define remove-at-tests:slow
  (test-suite+
   "Artificial remove-at tests for slow version of actions.rkt"
   (parameterize ([variant 0])
     (test-remove-at))))

(define remove-at-tests:fast
  (test-suite+
   "Artificial remove-at tests for fast version of actions.rkt"
   (parameterize ([variant 1])
     (test-remove-at))))

(bench-apply (basic-apply))
(time (run-tests example-tests:slow))
(time (run-tests example-tests:fast))
(time (run-tests list-set-tests:slow))
(time (run-tests list-set-tests:fast))
(time (run-tests remove-at-tests:slow))
(time (run-tests remove-at-tests:fast))
(basic-trace (bench-apply))