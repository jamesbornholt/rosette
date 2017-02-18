#lang agile

(provide get-pc-events
         pc-push! pc-pop!
         make-pc-event-reporter
         current-pc-events
         (struct-out pc-push-event)
         (struct-out pc-pop-event))

;; Tracking pc push and pop events.

;; make-pc-event-reporter : -> PCEventReporter
(define (make-pc-event-reporter)
  (box '()))

;; current-pc-events : (Parameterof PCEventReporter)
(define current-pc-events
  (make-parameter (make-pc-event-reporter)))

;; get-pc-events : PCEventReporter -> (Listof PCEvent)
(define (get-pc-events rep)
  (reverse (unbox rep)))

;; pc-push! : SymBool Metrics -> Void
(define (pc-push! pc metrics)
  (define b (current-pc-events))
  (set-box! b (cons (pc-push-event pc metrics) (unbox b))))

;; pc-pop! : Metrics -> Void
(define (pc-pop! metrics)
  (define b (current-pc-events))
  (set-box! b (cons (pc-pop-event metrics) (unbox b))))

(struct pc-push-event [pc metrics] #:transparent)
(struct pc-pop-event [metrics] #:transparent)
