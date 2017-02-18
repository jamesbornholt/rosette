#lang agile

(provide display-infeasible-pc-stats)

(require (only-in rosette/base/core/safe assert)
         (only-in rosette/base/form/control @and)
         (only-in rosette/solver/solution unsat?)
         (only-in rosette/query/form solve)
         "pc-event.rkt"
         "reporter.rkt")

;; type PCStack = (Listof PCStackFrame)
;; A PCStackFrame is one of:
;;  - (pc-stack-frame/infeasible Number)
;;  - (pc-stack-frame/feasible SymBool)
(struct pc-stack-frame/infeasible [start-time] #:transparent)
(struct pc-stack-frame/feasible [pc] #:transparent)

;; type InfeasiblePCInfo = (Listof InfeasiblePCTime)
;; A InfeasiblePCTime is a (infeasible-pc-time Number Number)
(struct infeasible-pc-time [start end] #:transparent)

;; display-infeasible-pc-stats : (Listof PCEvent) -> Void
(define (display-infeasible-pc-stats events)
  (printf "Computing feasibility of path conditions...\n")
  (display-infeasible-pc-info
   (compute-infeasible-pc-stats events '() '())))

;; compute-infeasible-pc-stats :
;; (Listof PCEvent) PCStack InfeasiblePCInfo -> InfeasiblePCInfo
(define (compute-infeasible-pc-stats events stack acc)
  (match events
    [(list)
     (reverse acc)]
    [(cons e es)
     (match e
       [(pc-push-event pc metrics)
        (define pc+ (@and (stack-existing-pc stack) pc))
        (compute-infeasible-pc-stats
         es
         (cons (if (or (stack-infeasible? stack) (pc-infeasible? pc+))
                   (pc-stack-frame/infeasible (metrics-time metrics))
                   (pc-stack-frame/feasible pc+))
               stack)
         acc)]
       [(pc-pop-event metrics)
        (match (first stack)
          [(pc-stack-frame/feasible _)
           (compute-infeasible-pc-stats
            es
            (rest stack)
            acc)]
          [(pc-stack-frame/infeasible start-time)
           (compute-infeasible-pc-stats
            es
            (rest stack)
            (cons (infeasible-pc-time start-time (metrics-time metrics))
                  acc))])])]))

;; stack-infeasible? : PCStack -> ConcreteBool
(define (stack-infeasible? stack)
  (and (not (empty? stack))
       (pc-stack-frame/infeasible? (first stack))))

;; pc-infeasible? : SymBool -> ConcreteBool
(define (pc-infeasible? pc)
  (unsat? (solve (assert pc))))

;; stack-existing-pc : PCStack -> SymBool
(define (stack-existing-pc stack)
  (match stack
    [(list) #true]
    [(cons (pc-stack-frame/infeasible _) _) #false]
    [(cons (pc-stack-frame/feasible pc) _) pc]))

;; display-infeasible-pc-info : InfeasiblePCInfo
(define (display-infeasible-pc-info info)
  (define total-time-spent-infeasible
    (for/sum ([ipt (in-list info)])
      (match-define (infeasible-pc-time start end) ipt)
      (- end start)))
  (printf "total time spent in infeasible paths: ~v\n"
          total-time-spent-infeasible))


