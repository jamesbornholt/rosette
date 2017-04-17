#lang racket

(provide (all-defined-out))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profiler data structures

;; A profile consists of a mutable box events, which contains a list of
;; profile-event? records.
(struct profile-state (events))

(struct profile-event ())
(struct profile-event-enter    profile-event     (location procedure inputs metrics))
(struct profile-event-exit     profile-event     (outputs metrics))
(struct profile-event-sample   profile-event     (metrics))
(struct profile-event-pc       profile-event     ())
(struct profile-event-pc-push  profile-event-pc  (pc metrics))
(struct profile-event-pc-pop   profile-event-pc  (metrics))

;; Returns a new profile
(define (make-profile-state)
  (profile-state (box '())))