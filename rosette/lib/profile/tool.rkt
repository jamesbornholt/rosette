#lang racket

(require "record.rkt" "reporter.rkt" 
         "renderer/renderer.rkt" "renderer/html.rkt")
(provide (all-defined-out))

; The selected renderer
(define current-renderer (make-parameter make-html-renderer))

; Executes the given thunk and prints the profile data generated during execution.
(define (profile-thunk thunk #:renderer [renderer% (current-renderer)]
                             #:source [source-stx #f]
                             #:name [name "Profile"])
  (define profile (make-profile))
  (define reporter (make-profiler-reporter))
  (define renderer (renderer% source-stx name (hash)))
  (start-renderer renderer profile reporter)
  (define-values (prof ret) (run-profile-thunk thunk profile reporter))
  (finish-renderer renderer prof)
  (apply values ret))


;; TODO:  we probably need a version of profile-thunk etc that does
;; the profiling wrt a clean symbolic state (empty assertion stack, term cache etc).


; Profile the given form
(define-syntax (profile stx)
  (syntax-case stx ()
    [(_ expr args ...)
     (syntax/loc stx
       (profile-thunk (thunk expr) #:source #'expr args ...))]))
