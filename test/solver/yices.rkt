#lang racket

(require rosette/solver/smt/yices
         (prefix-in solve/ "solve.rkt")
         (prefix-in solve+/ "solve+.rkt"))

(solve/run-tests-with (new yices%))
;(solve+/run-tests-with (new yices%))