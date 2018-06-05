#lang racket
 
(require racket/runtime-path (only-in rosette clear-state!)
         "base/solver.rkt" (only-in rosette/query/core current-solver)
         rosette/solver/smt/z3 rosette/solver/smt/cvc4 rosette/solver/smt/boolector)

(error-print-width 4)

(define-syntax run-all-tests
  (syntax-rules ()
    [(_ #:id id ([p1 v1] ...) t1 ...)
     (begin
       (let ([runner (dynamic-require t1 id)])
         (parameterize ([p1 v1] ...)
           (runner)
           (clear-state!))) ...)]
    [(_ ([p1 v1] ...) t1 ...)
     (run-all-tests #:id 'all-tests ([p1 v1] ...) t1 ...)]
    [(_ t1 ...)
     (run-all-tests #:id 'all-tests () t1 ...)]))
     

; tests that do not use the solver
(define (basic-tests)
  (run-all-tests
   "base/effects.rkt" 
   "base/type.rkt" 
   "base/term.rkt"
   "base/bool.rkt"
   "base/merge.rkt"
   "base/list.rkt"
   "base/vector.rkt"
   "base/forall.rkt"
   "base/ord-dict.rkt"))

; tests for basic SMT support (QF_BV)
(define (smt-basic-tests)
  (run-all-tests
   "base/bitvector.rkt" 
   "base/equality.rkt"
   "base/uninterpreted.rkt"))

; tests for other theories
(define (smt-advanced-tests)
  (run-all-tests
   "base/real.rkt"
   "base/quantified.rkt"
   "base/finitize.rkt"
   "base/distinct.rkt"
   "base/generics.rkt"))

; tests for query forms that require only basic SMT
(define (query-basic-tests)
  (run-all-tests
   "query/solve.rkt"
   "query/verify.rkt"
   "query/synthesize.rkt"
   "query/solve+.rkt"
   "query/synthax.rkt"
   "query/push-pop.rkt"))

; tests for query forms that require advanced SMT
(define (query-advanced-tests)
  (run-all-tests
   "query/debug.rkt"
   "query/optimize.rkt"))


; run all tests with z3
(parameterize ([current-solver (z3)][solver (z3)])
  (printf "===== testing with z3 =====\n")
  (basic-tests)
  (smt-basic-tests)
  (smt-advanced-tests)
  (query-basic-tests)
  (query-advanced-tests))

; run all solver tests except debug/optimize with cvc4 if available
(when (cvc4-available?)
  (parameterize ([current-solver (cvc4)][solver (cvc4)])
    (printf "===== testing with cvc4 =====\n")
    (smt-basic-tests)
    (smt-advanced-tests)
    (query-basic-tests)))

; run only basic solver tests for boolector
(when (boolector-available?)
  (parameterize ([current-solver (boolector)][solver (boolector)])
    (printf "===== testing with boolector =====\n")
    (smt-basic-tests)
    (query-basic-tests)))
