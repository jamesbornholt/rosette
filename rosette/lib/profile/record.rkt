#lang racket

(require rosette/base/core/reporter racket/hash
         "feature.rkt" "reporter.rkt")
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
(struct profile-node (parent children data)
  #:transparent #:mutable)

(struct profile-data (location procedure inputs outputs metrics start finish)
  #:transparent #:mutable)

;; Compute profile data on procedure entry
(define (entry-data loc proc in)
  (let ([start (get-current-metrics)])
    (profile-data loc proc (compute-features in) (hash) (hash) start (hash))))

;; Returns a new top-level profile node
(define (make-top-level-profile)
  (let ([start (hash)])
    (profile-node #f '() (profile-data 'top #f (hash) (hash) (hash) start (hash)))))

;; A parameter that holds the current profile / call stack.
(define current-profile (make-parameter (make-top-level-profile)))

;; A default reporter
(current-reporter (make-profiler-reporter))

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
                                 (time-apply (thunk (with-handlers ([exn:fail? handler])
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
  (let* ([data (entry-data loc proc in)]
         [node (profile-node (current-profile) '() data)])
    (set-profile-node-children! (current-profile) (cons node (profile-node-children (current-profile))))
    (current-profile node)))

(define (diff-metrics old new)
  (for/hash ([(k v) new]) (values k (- v (hash-ref old k 0)))))

;; Records a method exit by modifying the out, cpu, real, and gc fields
;; of the profile-node at the top of the current-profile-stack.
;; This procedure should be called after the profiled procedure call returns.
(define (record-exit! out cpu real gc)
  (let* ([entry (profile-node-data (current-profile))]
         [metrics (profile-data-metrics entry)])
    (set-profile-data-finish! entry (get-current-metrics))
    (set-profile-data-outputs! entry (compute-features out))
    (set-profile-data-metrics! entry (hash-union
                                      (hash 'cpu cpu 'real real 'gc gc)
                                      (diff-metrics (profile-data-start entry) (profile-data-finish entry))))
    (current-profile (profile-node-parent (current-profile)))))

;; Run a top-level profiled thunk and return a profile node for its extent
(define (run-profile-thunk proc)
  (define state (make-top-level-profile))
  (define reporter (make-profiler-reporter))
  (set-profile-node-data! state (entry-data 'top 'top '()))
  (define ret (parameterize ([current-profile state]
                             [current-reporter reporter])
                (define-values (out cpu real gc) (time-apply proc '()))
                (record-exit! out cpu real gc)
                out))
  (values state ret))

