#lang racket

(require "config.rkt")
(provide bench bench-define)

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
     

; The bench-define form creates a procedure that
; uses bench-apply to apply the provided body to
; the procedure's arguments.
(define-syntax (bench-define stx)
  (syntax-case stx ()
    [(_ (id arg ...) body ...)
     (syntax/loc stx
       (define id
         (let ([id (lambda (arg ...) body ...)])
           (lambda (arg ...)
             ((bench-apply) id (list arg ...))))))]))