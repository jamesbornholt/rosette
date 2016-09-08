#lang racket

(require (only-in rosette union? union-contents expression union)
         (only-in rosette/base/core/polymorphic guarded)
         (only-in rosette/base/core/type get-type typed? type-deconstruct))

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

; Updates the footprint map to contain the object graph of x.
; The footprint is a set of key-value pairs, where the key is an
; object (a node in the graph), and the value is the number of
; outgoing edges.
(define (measure! footprint x)
  (unless (hash-has-key? footprint x)
    (match x
      [(or (union children)
           (expression _ children ...)
           (? list? children))
       (hash-set! footprint x (length children))
       (for ([c children])
         (measure! footprint c))]
      [(guarded t c)
       (hash-set! footprint x 2)
       (measure! footprint t)
       (measure! footprint c)]
      [(cons a b)
       (hash-set! footprint x 2)
       (measure! footprint a)
       (measure! footprint b)]
      [(? vector?)
       (hash-set! footprint x (vector-length x))
       (for ([c x])
         (measure! footprint c))]
      [(box c) 
       (hash-set! footprint x 1)
       (measure! footprint c)]
      [(? typed?)
       (match (type-deconstruct (get-type x) x)
         [(list (== x)) (hash-set! footprint x 0)]
         [children
          (hash-set! footprint x (length children))
          (for ([c children])
            (measure! footprint c))])]
      [_ (hash-set! footprint x 0)])))

       
 
; A simple feature that measures V + E, where V is the number of vertices and
; E is the number of edges that make up the input object graph.
(define heap-size-feature
  (feature
   'heap-size
   (lambda (xs)
     (define footprint (make-hash))
     (for ([x xs]) (measure! footprint x))
     (+ (hash-count footprint)
        (for/sum ([v (in-hash-values footprint)]) v)))))
   

; A parameter that holds a list of features to profile.
(define current-features
  (make-parameter
   (list heap-size-feature union-size-feature expr-length-feature)
   (lambda (fs)
     (unless (and (list? fs) (andmap feature? fs))
       (error 'current-features "Expected a list of feature?, given ~a" fs))
     fs)))