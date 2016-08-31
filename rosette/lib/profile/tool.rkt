#lang racket

(require "record.rkt" "display.rkt" racket/pretty)
(provide (all-defined-out))

(require "renderer/complexity.rkt")

; Executes the given thunk and prints the profile data generated during execution.
(define (profile-thunk thunk #:renderer [renderer (complexity-renderer #:plot? #t)])
  (define state (make-profile-stack))
  (define ret (parameterize ([current-profile-stack state])
                (thunk)))
  (renderer (profile-stack-root state))
  ret)

;; TODO:  we probably need a version of profile-thunk etc that does
;; the profiling wrt a clean symbolic state (empty assertion stack, term cache etc).

;; TODO:  need an interface for controlling the output of profile-thunk.