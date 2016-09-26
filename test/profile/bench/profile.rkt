#lang racket

(require rosette/lib/profile rosette/lib/profile/renderer/html)
(provide profile-bench)

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
