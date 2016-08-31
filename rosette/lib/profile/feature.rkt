#lang racket

(require "record.rkt" (only-in rosette union? union-contents expression union))

(provide (all-defined-out))



; Stores a procedure that takes as input a profile-node
; and outputs a pair of numbers.  The first number in the
; pair characterizes the profiled input, and the second
; number characterizes the output.
(struct feature (name procedure)
  #:property prop:procedure
  [struct-field-index procedure]
  #:guard (lambda (name proc id)
            (unless (procedure? proc)
              (error 'feature "Expected a procedure?, given ~a" proc))
            (values name proc))
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "(feature ~a)" (feature-name self)))])


; A simple feature that returns the sum of the sizes of the input unions,
; and the sum of the sizes of the output unions.
(define union-size-feature
  (feature
   'union-size
   (let ([union-sizes (lambda (xs)
                        (for/sum ([x xs] #:when (union? x))
                          (length (union-contents x))))])
     (match-lambda
       [(profile-node _ _ inputs outputs _ _ _ _)
        (cons (union-sizes inputs)
              (union-sizes outputs))]
       [x (error 'union-size-feature "Expected a profile-node?, given ~a" x)]))))

; A simple feature that measures the maximum length of expressions in the input
; and output arguments.
(define expr-length-feature
  (feature
   'expr-length
   (letrec ([expr-length
             (lambda (e)
               (let ([ans (match e
                            [(expression op elts ...) (+ 1 (apply + (map expr-length elts)))]
                            [(union : (_ v) ...) (+ 1 (apply + (map expr-length v)))] ; TODO treat guards too
                            [(list elts ...) (apply + (map expr-length elts))]
                            [_ 1])])
                 ans))])
     (match-lambda
       [(profile-node _ _ inputs outputs _ _ _ _)
        (cons (if (null? inputs) 1 (apply max (map expr-length inputs)))
              (if (null? outputs) 1 (apply max (map expr-length outputs))))]
       [x (error 'expr-length-feature "Expected a profile-node?, given ~a" x)]))))


; A parameter that holds a list of features to profile.
(define current-features
  (make-parameter
   (list union-size-feature expr-length-feature)
   (lambda (fs)
     (unless (and (list? fs) (andmap feature? fs))
       (error 'current-features "Expected a list of feature?, given ~a" fs))
     fs)))
