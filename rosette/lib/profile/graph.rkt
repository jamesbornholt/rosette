#lang racket

(require racket/hash racket/struct
         "record.rkt" "reporter.rkt")
(provide (except-out (all-defined-out) diff-metrics))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profile graph data structures

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
(struct profile-data (location procedure inputs outputs metrics start finish) #:mutable)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion

;; Convert an instance of profile-state (i.e., a list of profile events) into a
;; dynamic call graph representation.
(define (profile-state->graph state)
  (define events (reverse (unbox (profile-state-events state))))
  (unless (profile-event-enter? (first events))
    (error 'profile-state->graph "expected first event to be profile-event-enter"))
  (define node #f)
  (for ([e events])
    (match e
      [(profile-event-enter loc proc in met)
       (define new-data (profile-data loc proc in (hash) (hash) met (hash)))
       (define new-node (profile-node node '() new-data))
       (if (false? node)
           (set-profile-node-parent! new-node new-node)
           (set-profile-node-children! node (cons new-node (profile-node-children node))))
       (set! node new-node)]
      [(profile-event-exit out met)
       (define data (profile-node-data node))
       (define metrics (diff-metrics (profile-data-start data) met))
       (set-profile-data-metrics! data metrics)
       (set-profile-data-outputs! data out)
       (set-profile-data-finish!  data met)
       (set-profile-node-children! node (reverse (profile-node-children node)))
       (set! node (profile-node-parent node))]))
  node)


;; Helper to compute the difference between entry and exit metrics
(define (diff-metrics old new)
  (define old-fields (struct->list old))
  (define new-fields (struct->list new))
  (apply metrics (for/list ([o old-fields][n new-fields]) (- n o))))
