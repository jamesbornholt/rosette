#lang rosette

(require "actions.rkt" "../bench.rkt" 
         rosette/lib/angelic rosette/lib/roseunit
         rackunit rackunit/text-ui)
  

(define (test-examples [depth 2] [len 2])
  (test-sat "Easy co test"
   (synth (sketch '((1) (2)) '(+) depth len) co-ex))
  (test-sat  "Easy clt test"
   (synth (sketch '((2) (1 1) (2 1)) '(+ *) depth len) clt-ex))
  (test-sat "Hard mf test"
   (synth  (sketch '((2) (1 1) (1 1 1) (1 1 2 1)) '(+ * =) depth len) mf-ex))
  (void))

(profile-bench "slow RuleSynth actions.rkt" (with-variant 0 (test-examples)))
(profile-bench "fast RuleSynth actions.rkt" (with-variant 1 (test-examples)))
