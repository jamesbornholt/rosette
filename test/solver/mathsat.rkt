#lang racket

(require rosette/solver/smt/mathsat
         (prefix-in solve/ "solve.rkt")
         (prefix-in solve+/ "solve+.rkt"))

(solve/run-tests-with (new mathsat%))
(solve+/run-tests-with (new mathsat%))