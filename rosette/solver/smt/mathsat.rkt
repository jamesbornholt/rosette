#lang racket

(require racket/runtime-path "smt.rkt" "../common/server.rkt")

(provide mathsat%)

(define-runtime-path mathsat (build-path ".." ".." ".." "bin" "mathsat"))

(define mathsat%
  (class* smt% (writable<%>) (inspect (make-inspector))

    (super-new [path mathsat] 
               [opts '("-input=smt2" "-model_generation=true")])
    
    (define/public (custom-write port) (fprintf port "mathsat%"))
    (define/public (custom-display port) (custom-write port))))
