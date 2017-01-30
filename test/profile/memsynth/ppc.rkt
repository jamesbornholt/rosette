#lang rosette

(require "litmus/lang.rkt" "litmus/sigs.rkt" "ocelot/ocelot.rkt"
         "alglave/execution.rkt" "alglave/sketch-model.rkt"
         "base.rkt")


(define-litmus-test test/ppc/safe455
  (((R x 1)
    (F sync)
    (R y 0))
   ((W y 1)
    (F sync)
    (W y 2))
   ((R y 2)
    (F lwsync)
    (R z 0))
   ((W z 1)
    (F lwsync)
    (W x 1)))
  #:post ((y 2))
  #:allowed vacuous)


(define (ppc-sketch)
  (define ppo (make-ppo-sketch 4 (list + - -> & SameAddr)
                                 (list po dp MemoryEvent Reads Writes)))
  (define grf (make-grf-sketch 4 (list + - -> & SameAddr)
                                 (list rfi rfe none univ)))
  (define ab-sync (make-ab-sketch 4 (list + join <: :>)
                                    (list rf Writes Reads (join (:> po Syncs) po))))
  (define ab-lwsync (make-ab-sketch 4 (list + join <: :>)
                                      (list rf Writes Reads (& (join (:> po Lwsyncs) po) (+ (-> Writes Writes) (-> Reads MemoryEvent))))))
  (define ab (+ (^ ab-sync) (^ ab-lwsync)))

  (values ppo grf ab))


; Synthesize a memory model that rejects this test.
(time (run-test test/ppc/safe455 ppc-sketch #f))
