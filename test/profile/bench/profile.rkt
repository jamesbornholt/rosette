#lang racket

(require (only-in rosette clear-state!)
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
          #:renderer (html-renderer)
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

(define-syntax-rule (profile-bench! args ...)
  (begin
    (profile-bench args ...)
    (clear-state!)))
