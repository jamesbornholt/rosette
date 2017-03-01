#lang racket

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

;; ------------------------------------------------------------------------

;; make-solver-gen : -> SolverGen
;; Uses the ∃-solve+ interface described in rosette/query/core.rkt
(define (make-solver-gen)
  (solve+))

;; gen-push : SolverGen SymBool -> Solution
;; Uses the ∃-solve+ interface described in rosette/query/core.rkt
;; to push a new constraint formula and get the new solution
(define (gen-push gen formula)
  (gen formula))

;; gen-pop : SolverGen -> Solution
;; Uses the ∃-solve+ interface described in rosette/query/core.rkt
;; to pop the last constraint formula
(define (gen-pop gen)
  (gen 1))

;; gen-shutdown : SolverGen -> Any
;; Uses the ∃-solve+ interface described in rosette/query/core.rkt
;; to shutdown the solver
(define (gen-shutdown gen)
  (gen 'shutdown))

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

;;; mutable!
;(define generator-restart 0)

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

;; a PCStack is one of:
;;  - (pc-stack-feasible (Listof SymBool))
;;  - (pc-stack-infeasible (Listof SymBool) Number Nat)
;; Where (pc-stack-feasible feas-pcs) represents a feasible stack, and
;; (pc-stack-infeasible feas-pcs start-time depth) represents an
;; infeasible stack beginning at time `start-time` that
;; has gone `depth` levels deeper into infeasible pcs.
;; A depth of zero signifies that this stack is at the
;; level where popping will result in a feasible stack.
(struct pc-stack-feasible [feas-pcs] #:transparent)
(struct pc-stack-infeasible [feas-pcs start-time depth] #:transparent)

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
  (define gen (make-solver-gen))
  (compute-infeasible-pc-stats/acc events cb gen (pc-stack-feasible '()) '()))

;; compute-infeasible-pc-stats/acc :
;; (Listof PCEvent) InfeasiblePCCallback SolverGen PCStack InfeasiblePCInfo -> InfeasiblePCInfo
;; At the end, this shuts gown the solver gen.
(define (compute-infeasible-pc-stats/acc events cb gen stack acc)
  (match events
    [(list)
     (gen-shutdown gen)
     (reverse acc)]
    [(cons e es)
     (match e
       [(pc-push-event pc metrics)
        (match stack
          [(pc-stack-feasible feas-pcs)
           (define new-gen
             (cond [(empty? feas-pcs)
                    (gen-shutdown gen)
                    ;(printf "generator restart: ~v\n" generator-restart)
                    ;(set! generator-restart (add1 generator-restart))
                    (make-solver-gen)]
                   [else
                    gen]))
           ;; The stack is feasible.
           ;; This pushes another formula and checks the resulting solution
           (cond
             [(pc-infeasible? new-gen pc)
              (compute-infeasible-pc-stats/acc
               es
               cb
               new-gen
               (pc-stack-infeasible feas-pcs (metrics-time metrics) 0)
               acc)]
             [else
              (compute-infeasible-pc-stats/acc
               es
               cb
               new-gen
               (pc-stack-feasible (cons pc feas-pcs)) ; the new stack is still feasible
               acc)])]
          [(pc-stack-infeasible feas-pcs start-time depth)
           ;; if the stack is already infeasible,
           ;; shouldn't need to push another formula
           (compute-infeasible-pc-stats/acc
            es
            cb
            gen
            (pc-stack-infeasible feas-pcs start-time (add1 depth))
            acc)])]
       [(pc-pop-event metrics)
        (match stack
          [(pc-stack-feasible feas-pcs)
           ;; the stack is feasible
           ;; pop the last constraint from the solver stack
           (gen-pop gen)
           (compute-infeasible-pc-stats/acc
            es
            cb
            gen
            (pc-stack-feasible (rest feas-pcs))
            acc)]
          [(pc-stack-infeasible feas-pcs start-time depth)
           (cond
             [(zero? depth)
              ;; the stack is shallowly infeasible, so this pop event will
              ;; exit the infeasible region.
              ;; pop the last constraint from the solver stack
              (gen-pop gen)
              (define ipt (infeasible-pc-time start-time (metrics-time metrics)))
              (cb ipt)
              (compute-infeasible-pc-stats/acc
               es
               cb
               gen
               (pc-stack-feasible feas-pcs) ; after this pop, the stack is feasible again
               (cons ipt acc))]
             [else
              ;; the stack is deeply infeasible, so this pop event will
              ;; stay in the infeasible regoin.
              ;; The last constraint wasn't pushed, so shouldn't pop
              (compute-infeasible-pc-stats/acc
               es
               cb
               gen
               (pc-stack-infeasible feas-pcs start-time (sub1 depth))
               acc)])])])]))

;; pc-infeasible? : SolverGen SymBool -> ConcreteBool
(define (pc-infeasible? gen pc)
  ;; lst-sol is a list containing one element, the solution
  (define-values [lst-sol cpu real gc]
    (time-apply gen-push (list gen pc)))
  (define res (unsat? (first lst-sol)))
  (begin0
    res
    (record-solving-stats! res cpu)
    (maybe-print-solving-stats)))

;; display-infeasible-pc-info : InfeasiblePCInfo
(define (display-infeasible-pc-info info)
  (define total-time-spent-infeasible
    (for/sum ([ipt (in-list info)])
      (match-define (infeasible-pc-time start end) ipt)
      (- end start)))
  (printf "total time spent in infeasible paths: ~v\n"
          total-time-spent-infeasible))


