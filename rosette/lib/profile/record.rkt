#lang racket

(require "feature.rkt" "metrics.rkt")
(provide (all-defined-out))

;; Represents the global map from procedure objects to their source locations (if known).
(define current-sources (make-parameter (make-hash)))

;; Records the mapping from the given procedure object to its source info.
;; Returns the procedure object.
(define (record-source! proc location)
  (hash-set! (current-sources) proc location)
  proc)

;; Represents a profile entry.  The location field stores the location at
;; which the given procedure is invoked.  The inputs and outputs fields are
;; hash maps from features to numbers.  For each feature in current-features,
;; they store the value of that feature for the given inputs and the corresponding
;; outputs.  The metrics field is a hash map from symbols to numbers, where each
;; symbol describes a performance metric collected during symbolic evaluation,
;; e.g., cpu time, real time, gc time, the number of merge invocations, the number
;; of unions and terms created, etc.
(struct profile-node (location procedure inputs outputs metrics children)
  #:transparent #:mutable)

;; Represents a profile stack.
(struct profile-stack ([frames #:mutable] root)
  #:transparent)

;; Returns a new profile stack.
(define (make-profile-stack)
  (let ([root (profile-node 'root #f (hash) (hash) (hash) '())])
    (profile-stack (list root) root)))

;; A parameter that holds the current profile / call stack.
(define current-profile-stack (make-parameter (make-profile-stack)))

;; Returns a hash map with entries of the form <k, v> where k is a
;; feature in current-features and v = (k xs).
(define (compute-features xs)
  (for/hash ([f (current-features)]) (values f (f xs))))

;; Records the application of a procedure proc at a location loc to
;; the given by-position and/or keyword arguments.
(define record-apply!
  (let* ([handler  (lambda (exn) (values exn null))]
         [returner (lambda e (values #f e))]
         [runner (lambda (loc proc in app-proc-to-in)
                   (record-enter! loc proc in)
                   ; `out` will always be a list of two values:
                   ; * If the application does not produce an exception, the first value is #f
                   ;   and the second is a list of the values returned from the application.
                   ; * If the application does produce an exception, the first value is that exception
                   ;   and the second is null.
                   ; This arrangement enables collecting timing data even from applications that
                   ; throw exceptions, which is common during symbolic evaluation.
                   ; TODO: should we annotate the profle-node somehow to indicate an exception was thrown?
                   (let-values ([(out cpu real gc)
                                 (time-apply (thunk (with-handlers ([exn? handler])
                                                      (call-with-values app-proc-to-in returner)))
                                             null)])
                     (record-exit! (cadr out) cpu real gc)
                     (if (false? (car out)) (apply values (cadr out)) (raise (car out)))))])   
  (make-keyword-procedure
   (lambda (kws kw-args loc proc . rest) 
     (runner loc proc (append rest kw-args) (thunk (keyword-apply proc kws kw-args rest))))
   (lambda (loc proc . rest) 
     (runner loc proc rest (thunk (apply proc rest)))))))

;; Records a procedure entry by pushing a fresh profile-node onto the
;; current-profile-stack.
;; This procedure should be called after all arguments to the profiled procedure have been evaluated, but
;; before the procedure is invoked.
(define (record-enter! loc proc in)
  (let* ([metrics (hash 'merge-count (merge-count)
                        'union-count (union-count)
                        'union-sum (union-sum)
                        'term-count (term-count))]
         [entry (profile-node loc proc (compute-features in) (hash) metrics '())]
         [parent (car (profile-stack-frames (current-profile-stack)))])
    (set-profile-node-children! parent (cons entry (profile-node-children parent)))
    (set-profile-stack-frames! (current-profile-stack) (cons entry (profile-stack-frames (current-profile-stack))))))

(define-syntax-rule (diff metrics metric)
  (- (metric) (hash-ref metrics 'metric)))

;; Records a method exit by modifying the out, cpu, real, and gc fields
;; of the profile-node at the top of the current-profile-stack.
;; This procedure should be called after the profiled procedure call returns.
(define (record-exit! out cpu real gc)
  (let* ([entry (car (profile-stack-frames (current-profile-stack)))]
         [metrics (profile-node-metrics entry)])
    (set-profile-node-outputs! entry (compute-features out))
    (set-profile-node-metrics! entry (hash 'cpu cpu 'real real 'gc gc
                                           'merge-count (diff metrics merge-count)
                                           'union-count (diff metrics union-count)
                                           'union-sum (diff metrics union-sum)
                                           'term-count (diff metrics term-count)))
    (set-profile-stack-frames! (current-profile-stack) (cdr (profile-stack-frames (current-profile-stack))))))
