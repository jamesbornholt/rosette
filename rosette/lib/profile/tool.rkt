#lang racket

(require "record.rkt" "renderer/complexity.rkt")
(provide (all-defined-out))

; Executes the given thunk and prints the profile data generated during execution.
(define (profile-thunk thunk #:renderer [renderer (complexity-renderer #:plot? #f)]
                             #:source [source-stx #f]
                             #:name [name "Profile"])
  (define-values (node ret) (run-profile-thunk thunk))
  (renderer node source-stx name)
  (apply values ret))

;; TODO:  we probably need a version of profile-thunk etc that does
;; the profiling wrt a clean symbolic state (empty assertion stack, term cache etc).


; Profile the given form
(define-syntax (profile stx)
  (syntax-case stx ()
    [(_ expr args ...)
     (syntax/loc stx
       (profile-thunk (thunk expr) #:source #'expr args ...))]))
