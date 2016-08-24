#lang racket

(require "config.rkt")
(provide bench)

; This form selects between the provided expressions
; based on the value of the (variant) parameter.  The
; first expression is taken to be the default implementation. 
(define-syntax (bench stx)
  (syntax-case stx ()
    [(_ e0) #'e0]
    [(_ e0 e ...)
     (with-syntax
         ([(i ...) (for/list ([idx (length (syntax->list #'(e ...)))]) (add1 idx))])
       (syntax/loc stx
         (case (variant)
           [(i) e] ...
           [else e0])))]))
     

