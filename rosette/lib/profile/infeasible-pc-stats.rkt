#lang agile

(provide display-infeasible-pc-stats ; takes list of PCEvent
         display-infeasible-pc-info ; takes list of InfeasiblePCTime
         compute-infeasible-pc-stats
         (struct-out infeasible-pc-time))

(require (only-in rosette/base/core/safe assert)
         (only-in rosette/base/form/control @and)
         (only-in rosette/solver/solution unsat unsat?)
         (only-in rosette/query/form solve solve+)
         "pc-event.rkt"
         "reporter.rkt")

;; ----------------------------------------------------------------------------

(module+ print-solving-stats
  (provide print-solving-stats?
           print-solving-stats
           record-solving-stats
           new-solving-stats))

;; Printing the number of pcs solved so far, to give more information about
;; what's wrong when it takes a long time to solve them.

;; print-solving-stats? : (Parameterof Boolean)
(define print-solving-stats? (make-parameter #false))

(define PRINTING-TIME-INTERVAL 15)

;; prev-t : (Boxof Number)
(define prev-t (box (current-seconds)))

;; maybe-print-solving-stats : -> Void
(define (maybe-print-solving-stats)
  (when (print-solving-stats?)
    (define prev (unbox prev-t))
    (when (<= (+ prev PRINTING-TIME-INTERVAL) (current-seconds))
      (set-box! prev-t (current-seconds))
      (print-solving-stats))))

;; ------------------------------------------------------------------------

;; record-solving-stats : (Parameterof (U #false SolvingStats))
(define record-solving-stats (make-parameter #false))

;; new-solving-stats : -> SolvingStats
(define (new-solving-stats)
  (solving-stats 0 0 0 0))

;; A SolvingStats is a (solving-stats Nat Nat Number)
(struct solving-stats
  [num-solved num-infeasible total-solver-time infeasible-solver-time]
  #:transparent #:mutable)

;; record-solving-stats! : Boolean Number -> Void
(define (record-solving-stats! infeasible? ∆t)
  (define stats (record-solving-stats))
  (match stats
    [#false (void)]
    [(solving-stats num num-infeas time infeas-time)
     (set-solving-stats-num-solved! stats (add1 num))
     (set-solving-stats-total-solver-time! stats (+ time ∆t))
     (when infeasible?
       (set-solving-stats-num-infeasible! stats (add1 num-infeas))
       (set-solving-stats-infeasible-solver-time! stats (+ infeas-time ∆t)))]))

;; print-solving-stats : -> Void
(define (print-solving-stats)
  (match-define (solving-stats num num-infeas time time-infeas)
    (record-solving-stats))
  (printf (string-append
           "num-solved:      ~v\n"
           "num-infeasible:  ~v\n"
           "time-solving:    ~v\n"
           "time-infeasible: ~v\n"
           "average-time:    ~v\n")
          num
          num-infeas
          time
          time-infeas
          (and (not (zero? num))
               (real->double-flonum (/ time num)))))

;; ----------------------------------------------------------------------------

;; type PCStack = (Listof PCStackFrame)
;; A PCStackFrame is one of:
;;  - (pc-stack-frame/infeasible Number)
;;  - (pc-stack-frame/infeasible-deep)
;;  - (pc-stack-frame/feasible SymBool)
(struct pc-stack-frame/infeasible [start-time] #:transparent)
(struct pc-stack-frame/infeasible-deep [] #:transparent)
(struct pc-stack-frame/feasible [pc] #:transparent)

;; type InfeasiblePCInfo = (Listof InfeasiblePCTime)
;; A InfeasiblePCTime is a (infeasible-pc-time Number Number)
(struct infeasible-pc-time [start end] #:transparent)

;; display-infeasible-pc-stats :
;; (Listof PCEvent) InfeasiblePCCallback -> Void
(define (display-infeasible-pc-stats events cb)
  (printf "Computing feasibility of path conditions...\n")
  (display-infeasible-pc-info
   (compute-infeasible-pc-stats events cb)))

;; compute-infeasible-pc-stats :
;; (Listof PCEvent) InfeasiblePCCallback -> InfeasiblePCInfo
(define (compute-infeasible-pc-stats events cb)
  (define gen (solve+))
  (compute-infeasible-pc-stats/acc events cb gen '() '()))

(define (gen-unsat v)
  (unsat))

;; compute-infeasible-pc-stats/acc :
;; (Listof PCEvent) InfeasiblePCCallback PCStack SolverGen InfeasiblePCInfo -> InfeasiblePCInfo
;; ASSUME that gen is not dead
(define (compute-infeasible-pc-stats/acc events cb gen stack acc)
  (match events
    [(list)
     (reverse acc)]
    [(cons e es)
     (match e
       [(pc-push-event pc metrics)
        (cond
          [(stack-infeasible? stack)
           (compute-infeasible-pc-stats/acc
            es
            cb
            gen-unsat
            (cons (pc-stack-frame/infeasible-deep) stack)
            acc)]
          [(pc-infeasible? gen pc)
           (compute-infeasible-pc-stats/acc
            es
            cb
            ; TODO: This gen is now dead. Is this right?
            gen-unsat
            (cons (pc-stack-frame/infeasible (metrics-time metrics)) stack)
            acc)]
          [else
           (define pc+ (@and (stack-existing-pc stack) pc))
           (compute-infeasible-pc-stats/acc
            es
            cb
            gen
            (cons (pc-stack-frame/feasible pc+) stack)
            acc)])]
       [(pc-pop-event metrics)
        (define pop-stack (rest stack))
        ;; close the existing gen before possibly creating a new one
        (gen (list #false))
        (define pop-gen
          (stack-make-gen pop-stack))
        (match (first stack)
          [(pc-stack-frame/feasible _)
           (compute-infeasible-pc-stats/acc
            es
            cb
            pop-gen
            pop-stack
            acc)]
          [(pc-stack-frame/infeasible start-time)
           (define ipt (infeasible-pc-time start-time (metrics-time metrics)))
           (cb ipt)
           (compute-infeasible-pc-stats/acc
            es
            cb
            pop-gen
            pop-stack
            (cons ipt acc))]
          [(pc-stack-frame/infeasible-deep)
           (compute-infeasible-pc-stats/acc
            es
            cb
            pop-gen
            pop-stack
            acc)])])]))

;; stack-infeasible? : PCStack -> ConcreteBool
(define (stack-infeasible? stack)
  (and (not (empty? stack))
       (or (pc-stack-frame/infeasible? (first stack))
           (pc-stack-frame/infeasible-deep? (first stack)))))

;; pc-infeasible? : SolverGen SymBool -> ConcreteBool
(define (pc-infeasible? gen pc)
  ;; lst-sol is a list containing one element, the solution
  (define-values [lst-sol cpu real gc]
    (time-apply gen (list (list pc))))
  (define res (unsat? (first lst-sol)))
  (begin0
    res
    (record-solving-stats! res cpu)
    (maybe-print-solving-stats)))

;; stack-existing-pc : PCStack -> SymBool
(define (stack-existing-pc stack)
  (match stack
    [(list) #true]
    [(cons (pc-stack-frame/infeasible _) _) #false]
    [(cons (pc-stack-frame/infeasible-deep) _) #false]
    [(cons (pc-stack-frame/feasible pc) _) pc]))

;; stack-make-gen : PCStack -> SolverGen
(define (stack-make-gen stack)
  (match stack
    [(list) (solve+)]
    [(cons (pc-stack-frame/infeasible _) _) gen-unsat]
    [(cons (pc-stack-frame/infeasible-deep) _) gen-unsat]
    [(cons (pc-stack-frame/feasible pc) _)
     (define gen (solve+))
     (gen (list pc))
     gen]))

;; display-infeasible-pc-info : InfeasiblePCInfo
(define (display-infeasible-pc-info info)
  (define total-time-spent-infeasible
    (for/sum ([ipt (in-list info)])
      (match-define (infeasible-pc-time start end) ipt)
      (- end start)))
  (printf "total time spent in infeasible paths: ~v\n"
          total-time-spent-infeasible))


