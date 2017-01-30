#lang rosette

(require "litmus/sigs.rkt" "ocelot/ocelot.rkt"
         "alglave/execution.rkt" "alglave/axioms.rkt")
(provide run-test)

(define (run-test test sketch-factory outcome?)
  ; Create the sketch
  (define-values (ppo grf ab) (sketch-factory))

  ; Create the bounds for the test and execution
  (define bTest (instantiate-test test))
  (define iTest (instantiate-bounds bTest))
  (define bExec (make-execution bTest))
  (define iExec (instantiate-bounds bExec))
  (define interp (interpretation-union iTest iExec))

  ; Create the relational predicate
  (define VE (ValidExecution rf ws ppo grf ab #f))

  ; Interpret the relational predicate on the bounds
  (define-values (VE* assumes)
    (with-asserts (interpret* VE interp #:cache? #t)))

  ; Solve the predicate
  (define xs (symbolics iExec))
  (define asst (if outcome? VE* (forall xs (! VE*))))
  (sat? (solve asst)))
