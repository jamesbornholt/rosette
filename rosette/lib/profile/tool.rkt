#lang racket

(require "record.rkt" "display.rkt")
(provide (all-defined-out))

; Executes the given thunk and prints the profile data generated during execution.
(define (profile-thunk thunk)
  (define state (make-profile-stack))
  (define ret (parameterize ([current-profile-stack state])
                (thunk)))
  (for ([c (profile-node-children (profile-stack-root state))])
    (display-profile c))
  ret)

;; TODO:  we probably need a version of profile-thunk etc that does
;; the profiling wrt a clean symbolic state (empty assertion stack, term cache etc).