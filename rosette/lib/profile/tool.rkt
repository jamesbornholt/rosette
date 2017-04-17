#lang racket

(require "data.rkt" "record.rkt" "reporter.rkt"
         "infeasible-pc-stats.rkt"
         (submod "infeasible-pc-stats.rkt" print-solving-stats)
         "renderer/renderer.rkt" "renderer/renderer-infeasible-pc.rkt"
         "renderer/html.rkt")
(provide (all-defined-out))

; The selected renderer
(define current-renderer (make-parameter make-html-renderer))

;; compute-infeasible-pcs? : (Parameterof Boolean)
;; Determines whether it calculates feasibility of path conditions
(define compute-infeasible-pcs? (make-parameter #false))

; Executes the given thunk and prints the profile data generated during execution.
(define (profile-thunk thunk #:renderer [renderer% (current-renderer)]
                             #:source [source-stx #f]
                             #:name [name "Profile"])
  (define profile (make-profile-state))
  (define reporter (make-profiler-reporter profile))
  (define renderer (renderer% source-stx name))
  (start-renderer renderer profile reporter)
  (define-values (prof ret)
    (run-profile-thunk thunk profile reporter))
  (done-running renderer)
  (cond
    [(and (compute-infeasible-pcs?) (renderer/infeasible-pc? renderer))
     (finish-renderer/infeasible-pc
      renderer
      prof
      (parameterize ([record-solving-stats (new-solving-stats)])
        (begin0
          (time (compute-infeasible-pc-stats
                 prof
                 (get-infeasible-pc-callback renderer)))
          (print-solving-stats))))]
    [else
     (finish-renderer renderer prof)])
  (apply values ret))


;; TODO:  we probably need a version of profile-thunk etc that does
;; the profiling wrt a clean symbolic state (empty assertion stack, term cache etc).


; Profile the given form
(define-syntax (profile stx)
  (syntax-case stx ()
    [(_ expr args ...)
     (syntax/loc stx
       (profile-thunk (thunk expr) #:source #'expr args ...))]))
