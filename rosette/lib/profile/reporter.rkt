#lang racket

(require rosette/base/core/reporter racket/hash "pc-event.rkt")
(provide (struct-out profiler-reporter) (struct-out metrics) make-profiler-reporter get-current-metrics)

; The profiler reporter keeps a cumulative count of several metrics, and reports
; them when requested to insert into a profile node.
(define (make-profiler-reporter)
  (profiler-reporter
   (make-hash
    (map (curryr cons 0) '(term-count merge-count union-count union-size pc)))))

(struct profiler-reporter (metrics)
  #:transparent
  #:property prop:procedure
  (lambda (self . rest)
    (match rest
      [(list 'new-term)
       (inc! self 'term-count 1)]
      [(list 'merge)
       (inc! self 'merge-count 1)]
      [(list 'new-union union-size)
       (inc! self 'union-count 1)
       (inc! self 'union-size union-size)]
      [(list 'push-pc new-pc)
       (pc-push! new-pc (get-current-metrics #:reporter self))]
      [(list 'pop-pc)
       (pc-pop! (get-current-metrics #:reporter self))]
      [_ void])))

(define-syntax-rule (inc! reporter key val)
  (let ([ht (profiler-reporter-metrics reporter)])
    (hash-set! ht key (+ (hash-ref ht key 0) val))))
(define-syntax-rule (dec! reporter key val)
  (let ([ht (profiler-reporter-metrics reporter)])
    (hash-set! ht key (- (hash-ref ht key 0) val))))


; The metrics we use in profiles
(struct metrics (term-count merge-count union-count union-size cpu real gc time)
  #:transparent)

(define (get-current-metrics [cpu 0] [real 0] [gc 0] #:reporter [reporter (current-reporter)])
  (let ([mets (profiler-reporter-metrics reporter)])
    (metrics (hash-ref mets 'term-count)
             (hash-ref mets 'merge-count)
             (hash-ref mets 'union-count)
             (hash-ref mets 'union-size)
             cpu real gc (current-inexact-milliseconds))))
