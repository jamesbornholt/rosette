#lang racket

(require (only-in rosette union? union-contents expression union))

(provide (all-defined-out))



; Stores a procedure that takes as input a list of values
; and outputs a number that characterizes the cost of those values.   
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


; A simple feature that returns the sum of the sizes of the input unions.
(define union-size-feature
  (feature
   'union-size
   (lambda (xs)
     (for/sum ([x xs] #:when (union? x))
       (length (union-contents x))))))

; A simple feature that measures the maximum length of expressions in the input.
(define expr-length-feature
  (feature
   'expr-length
   (let ([cache (make-hash)])
     (letrec ([expr-length
               (lambda (e)
                 (if (hash-has-key? cache e)
                     (hash-ref cache e)
                     (let ([ans (match e
                                  [(expression op elts ...) (+ 1 (apply + (map expr-length elts)))]
                                  [(union : (_ v) ...) (+ 1 (apply + (map expr-length v)))] ; TODO treat guards too
                                  [(list elts ...) (apply + (map expr-length elts))]
                                  [_ 1])])
                       (hash-set! cache e ans)
                       ans)))])
       (lambda (xs)
         (if (null? xs) 1 (apply max (map expr-length xs))))))))


; A parameter that holds a list of features to profile.
(define current-features
  (make-parameter
   (list union-size-feature expr-length-feature)
   (lambda (fs)
     (unless (and (list? fs) (andmap feature? fs))
       (error 'current-features "Expected a list of feature?, given ~a" fs))
     fs)))
