#lang racket

(require racket/runtime-path "smt.rkt" "../common/server.rkt")

(provide boolector%)

(define-runtime-path boolector (build-path ".." ".." ".." "bin" "boolector"))

(define boolector%
  (class* smt% (writable<%>) (inspect (make-inspector))

    (super-new [path boolector] 
               [opts '("--smt2" "--smt2-model" "-i")])
    
    (define/public (custom-write port) (fprintf port "boolector%"))
    (define/public (custom-display port) (custom-write port))))