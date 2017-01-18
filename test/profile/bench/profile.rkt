#lang racket

(require (only-in rosette current-oracle oracle clear-asserts! clear-terms! current-solver)
         (only-in rosette/solver/smt/z3 z3)
         rosette/lib/profile rosette/lib/profile/renderer/html)
(provide profile-bench profile-bench! profile-bench-stream)

; Shorthand to profile a given expression and generate a HTML report.
(define-syntax (profile-bench stx)
  (syntax-case stx ()
    [(_ name body)
     (quasisyntax/loc stx
       (let ([n name])
         (printf "Profiling ~a...\n" n)
         (profile 
          #,(syntax/loc #'body
              (time body))
          #:name n)))]))

; Shorthand to profile a given expression and generate a HTML report.
(define-syntax (profile-bench-stream stx)
  (syntax-case stx ()
    [(_ name body)
     (quasisyntax/loc stx
       (let ([n name])
         (printf "Profiling ~a...\n" n)
         (profile-stream
          #,(syntax/loc #'body
              (time body))
          #:name n)))]))

(define (clear-most-state!)
  (current-oracle (oracle))
  (clear-asserts!)
  (clear-terms!)
  (current-solver (z3)))

(define-syntax-rule (profile-bench! args ...)
  (begin
    (profile-bench args ...)
    (clear-most-state!)))
