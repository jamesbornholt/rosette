#lang racket

(require "feature.rkt")
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
;; outputs.
(struct profile-node (location procedure inputs outputs cpu real gc children)
  #:transparent #:mutable)

;; Represents a profile stack.
(struct profile-stack ([frames #:mutable] root)
  #:transparent)

;; Returns a new profile stack.
(define (make-profile-stack)
  (let ([root (profile-node 'root #f (hash) (hash) #f #f #f '())])
    (profile-stack (list root) root)))

;; A parameter that holds the current profile / call stack.
(define current-profile-stack (make-parameter (make-profile-stack)))

;; Returns a hash map with entries of the form <k, v> where k is a
;; feature in current-features and v = (k xs).
(define (compute-features xs)
  (for/hash ([f (current-features)]) (values f (f xs))))

;; Records a procedure entry by pushing a fresh profile-node onto the
;; current-profile-stack.
;; This procedure should be called after all arguments to the profiled procedure have been evaluated, but
;; before the procedure is invoked.
(define (record-enter! loc proc in)
  (let ([entry (profile-node loc proc (compute-features in) (hash) #f #f #f '())]
        [parent (car (profile-stack-frames (current-profile-stack)))])
    (set-profile-node-children! parent (cons entry (profile-node-children parent)))
    (set-profile-stack-frames! (current-profile-stack) (cons entry (profile-stack-frames (current-profile-stack))))))

;; Records a method exit by modifying the out, cpu, real, and gc fields
;; of the profile-node at the top of the current-profile-stack.
;; This procedure should be called after the profiled procedure call returns.
(define (record-exit! out cpu real gc)
  (let ([entry (car (profile-stack-frames (current-profile-stack)))])
    (set-profile-node-outputs! entry (compute-features out))
    (set-profile-node-cpu! entry cpu)
    (set-profile-node-real! entry real)
    (set-profile-node-gc! entry gc)
    (set-profile-stack-frames! (current-profile-stack) (cdr (profile-stack-frames (current-profile-stack))))))
