#lang racket

(require racket/runtime-path "smt.rkt" "../common/server.rkt")

(provide yices%)

(define-runtime-path yices (build-path ".." ".." ".." "bin" "yices-smt2"))

(define yices%
  (class* smt% (writable<%>) (inspect (make-inspector))

    (super-new [path yices] 
               [opts '("--incremental")])
    
    (define/public (custom-write port) (fprintf port "yices%"))
    (define/public (custom-display port) (custom-write port))))
