#lang racket

(provide (all-defined-out))

;; Represents a profile entry.
(struct profile-node (location procedure inputs outputs cpu real gc children)
  #:transparent #:mutable)

;; Represents a profile stack.
(struct profile-stack ([frames #:mutable] root)
  #:transparent)

;; Returns a new profile stack.
(define (make-profile-stack)
  (let ([root (profile-node 'root #f '() '() #f #f #f '())])
    (profile-stack (list root) root)))

;; A parameter that holds the current profile / call stack.
(define current-profile-stack (make-parameter (make-profile-stack)))

;; Records a procedure entry by pushing a fresh profile-node onto the
;; current-profile-stack.
;; This procedure should be called after all arguments to the profiled procedure have been evaluated, but
;; before the procedure is invoked.
(define (record-enter! loc proc in)
  (let ([entry (profile-node loc proc in #f #f #f #f '())]
        [parent (car (profile-stack-frames (current-profile-stack)))])
    (set-profile-node-children! parent (cons entry (profile-node-children parent)))
    (set-profile-stack-frames! (current-profile-stack) (cons entry (profile-stack-frames (current-profile-stack))))))

;; Records a method exit by modifying the out, cpu, real, and gc fields
;; of the profile-node at the top of the current-profile-stack.
;; This procedure should be called after the profiled procedure call returns.
(define (record-exit! out cpu real gc)
  (let ([entry (car (profile-stack-frames (current-profile-stack)))])
    (set-profile-node-outputs! entry out)
    (set-profile-node-cpu! entry cpu)
    (set-profile-node-real! entry real)
    (set-profile-node-gc! entry gc)
    (set-profile-stack-frames! (current-profile-stack) (cdr (profile-stack-frames (current-profile-stack))))))
