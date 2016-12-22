#lang racket

(require rosette/base/core/reporter racket/hash)
(provide (struct-out profiler-reporter) make-profiler-reporter get-current-metrics)

; The profiler reporter keeps a cumulative count of several metrics, and reports
; them when requested to insert into a profile node.
(define (make-profiler-reporter)
  (profiler-reporter
   (make-hash (map (curryr cons 0) '(term-count merge-count union-count union-size)))))

(struct profiler-reporter (metrics)
  #:transparent
  #:property prop:procedure
  (lambda (self . rest)
    (match rest
      [(list 'new-term)
       (incr! self 'term-count 1)]
      [(list 'merge)
       (incr! self 'merge-count 1)]
      [(list 'new-union union-size)
       (incr! self 'union-count 1)
       (incr! self 'union-size union-size)]
      [_ void])))

(define-syntax-rule (incr! reporter key val)
  (let ([ht (profiler-reporter-metrics reporter)])
    (hash-set! ht key (+ (hash-ref ht key 0) val))))

(define (get-current-metrics [reporter (current-reporter)])
  (hash-union (hash 'time (current-inexact-milliseconds))
              (profiler-reporter-metrics reporter)))
