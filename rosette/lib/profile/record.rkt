#lang racket

(require rosette/base/core/reporter racket/hash
         "feature.rkt" "reporter.rkt")
(provide (all-defined-out))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source location tracking

;; Represents the global map from procedure objects to their source locations (if known).
(define current-sources (make-parameter (make-hash)))

;; Records the mapping from the given procedure object to its source info.
;; Returns the procedure object.
(define (record-source! proc location)
  (hash-set! (current-sources) proc location)
  proc)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profiler data structures

;; A profile consists of a mutable box events, which contains a list of
;; profile-event? records.
(struct profile-state (events))

(struct profile-event ())
(struct profile-event-enter (location procedure inputs metrics))
(struct profile-event-exit  (outputs metrics))
(struct profile-event-sample (metrics))

;; Returns a new profile
(define (make-profile-state)
  (profile-state (box '())))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profiler run-time state

;; A parameter that holds the current profile / call stack.
(define current-profile (make-parameter (make-profile-state)))

;; A default reporter
(current-reporter (make-profiler-reporter))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run-time profile node updates

;; Records a procedure entry by appending a new profile node to the current one.
;; This procedure should be called after all arguments to the profiled procedure
;; have been evaluated, but before the procedure is invoked.
(define (record-enter! loc proc in)
  (let* ([curr (current-profile)]
         [inputs (compute-features in)]
         [events (profile-state-events curr)]
         [metrics (get-current-metrics)]
         [old (unbox events)]
         [new (cons (profile-event-enter loc proc inputs metrics) old)])
    (let loop ()
      (unless (box-cas! events old new)
        (loop)))))

;; Records a procedure exit by modifying the data for the current profile node.
;; This procedure should be called after the profiled procedure call returns.
(define (record-exit! out cpu real gc)
  (let* ([curr (current-profile)]
         [outputs (compute-features out)]
         [metrics (get-current-metrics cpu real gc)]
         [events (profile-state-events curr)]
         [old (unbox events)]
         [new (cons (profile-event-exit outputs metrics) old)])
    (let loop ()
      (unless (box-cas! events old new)
        (loop)))))

;; Helper to compute the difference between entry and exit metrics
(define (diff-metrics old new)
  (for/hash ([(k v) new]) (values k (- v (hash-ref old k 0)))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level profiler invocation

;; Run a thunk and return a profile for its extent
(define (run-profile-thunk proc profile reporter)
  (define ret (parameterize ([current-profile profile]
                             [current-reporter reporter])
                (record-enter! 'top #f (hash))
                (define-values (out cpu real gc) (time-apply proc '()))
                (record-exit! out cpu real gc)
                out))
  (values profile ret))
