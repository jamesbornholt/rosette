#lang agile

(provide display-infeasible-pc-stats ; takes list of PCEvent
         display-infeasible-pc-info ; takes list of InfeasiblePCTime
         compute-infeasible-pc-stats
         (struct-out infeasible-pc-time))

(require (only-in rosette/base/core/safe assert)
         (only-in rosette/base/form/control @and)
         (only-in rosette/solver/solution unsat?)
         (only-in rosette/query/form solve)
         "pc-event.rkt"
         "reporter.rkt")

;; ----------------------------------------------------------------------------

;; Printing the number of pcs solved so far, to give more information about
;; what's wrong when it takes a long time to solve them.

(define print-num-solved? (make-parameter #false))

(define NUM-SOLVED-PRINTING-TIME-INTERVAL 15)

(define num-solved (box 0))
(define prev-t (box (current-seconds)))

(define (num-solved-inc!)
  (define num (unbox num-solved))
  (box-cas! num-solved num (add1 num)))

(define (maybe-print-num-solved)
  (when (print-num-solved?)
    (define prev (unbox prev-t))
    (when (<= (+ prev NUM-SOLVED-PRINTING-TIME-INTERVAL) (current-seconds))
      (box-cas! prev-t prev (current-seconds))
      (printf "num-solved: ~v\n" (unbox num-solved)))))

(module+ num-solved
  (provide print-num-solved?))

;; ----------------------------------------------------------------------------

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
   (compute-infeasible-pc-stats events)))

;; compute-infeasible-pc-stats : (Listof PCEvent) -> InfeasiblePCInfo
(define (compute-infeasible-pc-stats events)
  (compute-infeasible-pc-stats/acc events '() '()))

;; compute-infeasible-pc-stats/acc :
;; (Listof PCEvent) PCStack InfeasiblePCInfo -> InfeasiblePCInfo
(define (compute-infeasible-pc-stats/acc events stack acc)
  (match events
    [(list)
     (reverse acc)]
    [(cons e es)
     (match e
       [(pc-push-event pc metrics)
        (define pc+ (@and (stack-existing-pc stack) pc))
        (compute-infeasible-pc-stats/acc
         es
         (cons (if (or (stack-infeasible? stack) (pc-infeasible? pc+))
                   (pc-stack-frame/infeasible (metrics-time metrics))
                   (pc-stack-frame/feasible pc+))
               stack)
         acc)]
       [(pc-pop-event metrics)
        (match (first stack)
          [(pc-stack-frame/feasible _)
           (compute-infeasible-pc-stats/acc
            es
            (rest stack)
            acc)]
          [(pc-stack-frame/infeasible start-time)
           (compute-infeasible-pc-stats/acc
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
  (begin0
    (unsat? (solve (assert pc)))
    (num-solved-inc!)
    (maybe-print-num-solved)))

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


