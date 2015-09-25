#lang racket

(require rosette/solver/smt/boolector 
         (prefix-in solve/ "solve.rkt")
         (prefix-in solve+/ "solve+.rkt"))

(solve/run-tests-with (new boolector%))
(solve+/run-tests-with (new boolector%))