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

;; A profile consists of a root profile node and some auxiliary data
;; used for streaming the profile.
(struct profile-state (root curr stream) #:mutable)


;; A profile node is an entry in the dynamic control flow graph of the
;; profiled code. It contains a pointer to its parent node,
;; a list of children nodes, and a profile-data struct that contains the
;; actual data for the profile.
(struct profile-node (parent children data) #:mutable)


;; Profile data for a single procedure invocation.
;; * The location field stores the location at which the given procedure was
;;   invoked.
;; * The procedure field is the invoked procedure
;; * The inputs and outputs fields are hash maps from features to numbers.
;;   For each feature in current-features, they store the value of that
;;   feature for the inputs and outputs of the current invocation.
;; * The metrics field is a hash map from symbols to numbers, where each
;;   symbol describes a performance metric collected during symbolic evaluation,
;;   e.g., cpu time, real time, gc time, the number of merge invocations, the number
;;   of unions and terms created, etc.
;; * The start and finish fields track the value of various metrics at the entry
;;   and exit to the current invocation, respectively.
(struct profile-data (location procedure inputs outputs metrics start finish)
  #:mutable)


;; Profile streaming data consists of a list of nodes, a map from nodes
;; to their ID, a list of edges (pairs of node IDs), and a list of nodes
;; that have been exited (used to track which nodes need to be updated if
;; output is streamed).
(struct profile-stream (nodes node->idx edges finished)
  #:mutable)


;; Returns a new profile
(define (make-profile)
  (let* ([start (hash)]
         [data (profile-data 'top #f (hash) (hash) (hash) start (hash))]
         [node (profile-node #f '() data)]
         [stream (make-profile-stream node)])
    (profile-state node node stream)))


;; Returns a new profile stream with a given root profile node
(define (make-profile-stream root)
  (profile-stream '() (make-hash (list (cons root 0))) '() '()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profiler run-time state

;; A parameter that holds the current profile / call stack.
(define current-profile (make-parameter (make-profile)))

;; A default reporter
(current-reporter (make-profiler-reporter))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run-time profile node updates

;; Records a procedure entry by appending a new profile node to the current one.
;; This procedure should be called after all arguments to the profiled procedure
;; have been evaluated, but before the procedure is invoked.
(define (record-enter! loc proc in)
  (let* ([curr (current-profile)]
         [curr-stream (profile-state-stream curr)]
         [curr-node (profile-state-curr curr)]
         [new-data (entry-data loc proc in)]
         [new-node (profile-node curr-node '() new-data)]
         [node->idx (profile-stream-node->idx curr-stream)]
         [parent-idx (hash-ref node->idx curr-node)]
         [new-idx (hash-count node->idx)])
    (set-profile-stream-nodes! curr-stream (cons new-node
                                                 (profile-stream-nodes curr-stream)))
    (hash-set! node->idx new-node new-idx)
    (set-profile-stream-edges! curr-stream (cons (cons parent-idx new-idx)
                                                 (profile-stream-edges curr-stream)))
    (set-profile-node-children! curr-node (cons new-node
                                                (profile-node-children curr-node)))
    (set-profile-state-curr! curr new-node)))

;; Helper to compute profile data on procedure entry
(define (entry-data loc proc in [rep (current-reporter)])
  (let ([start (get-current-metrics rep)])
    (profile-data loc proc (compute-features in) (hash) (hash) start (hash))))


;; Records a procedure exit by modifying the data for the current profile node.
;; This procedure should be called after the profiled procedure call returns.
(define (record-exit! out cpu real gc)
  (let* ([curr (current-profile)]
         [curr-stream (profile-state-stream curr)]
         [curr-node (profile-state-curr curr)]
         [node->idx (profile-stream-node->idx curr-stream)]
         [data (profile-node-data curr-node)]
         [metrics (profile-data-metrics data)])
    (set-profile-data-finish! data (get-current-metrics))
    (set-profile-data-outputs! data (compute-features out))
    (set-profile-data-metrics! data (hash-union
                                     (hash 'cpu cpu 'real real 'gc gc)
                                     (diff-metrics (profile-data-start data) (profile-data-finish data))))
    (set-profile-stream-finished! curr-stream (cons (hash-ref node->idx curr-node)
                                                    (profile-stream-finished curr-stream)))
    (set-profile-state-curr! curr (profile-node-parent curr-node))))

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
  (set-profile-node-data! (profile-state-curr profile) (entry-data 'top 'top '() reporter))
  (define ret (parameterize ([current-profile profile]
                             [current-reporter reporter])
                (define-values (out cpu real gc) (time-apply proc '()))
                (record-exit! out cpu real gc)
                out))
  (values profile ret))
