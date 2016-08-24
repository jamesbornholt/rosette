#lang racket

(require "config.rkt" (only-in rosette union? union-contents))
(provide (all-defined-out))


(struct call (proc in out time settings) #:transparent)

; A very basic stats collector implementation for bench-apply.
; It records the procedure that was called, all of its arguments,
; the result, and the running time, as given by time-apply.  The
; collector acts as a procedure.  Every time the procedure is called,
; it updates the calls field with a call record.  These records can
; be obtained by calling basic-apply-calls.
(struct basic-apply ([calls #:mutable #:auto]) #:transparent
  #:auto-value '()
  #:property prop:procedure
  (lambda (self proc args)
    (let-values ([(settings) (list (variant) (merge-structs?))]
                 [(out cpu real gc) (time-apply proc args)])
      (set-basic-apply-calls!
       self (cons (call proc args (car out) (list cpu real gc) settings)
                  (basic-apply-calls self)))
      (car out))))

; Returns the union size if v is a union, otherwise returns #f.
(define (basic-info v)
  (and (union? v) (length (union-contents v))))

; Prints simple information about the given basic-apply value
; (in particular, if any of the arguments are unions, prints their
; size, otherwise prints #f for that argument).
(define (basic-trace app)
  (match app
    [(basic-apply (app reverse calls))
     (for ([c calls])
       (match c
         [(call proc in out (list cpu real gc) (list variant merge-structs?))
          (printf "~a: ~a -> ~a, variant: ~a, merge-structs?: ~a, cpu: ~a, real: ~a, gc: ~a\n"
                  (object-name proc)
                  (map basic-info in)
                  (basic-info out)
                  variant merge-structs?
                  cpu real gc)]))]))