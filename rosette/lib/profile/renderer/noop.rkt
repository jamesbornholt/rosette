#lang racket

(require "../data.rkt" "../record.rkt" "renderer.rkt")
(provide make-noop-renderer)

; The noop renderer does nothing!

(define (make-noop-renderer source name [options (hash)])
  (noop-renderer source name))

(struct noop-renderer (source name) 
  #:transparent
  #:methods gen:renderer
  [(define start-renderer void)
   (define done-running void)
   (define (finish-renderer self profile)
     (printf "Profiled ~v events.\n" (length (unbox (profile-state-events profile)))))])
