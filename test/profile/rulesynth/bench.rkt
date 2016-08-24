#lang rosette

(require rosette/lib/angelic)
(require (prefix-in slow- (only-in "slow.rkt" list-set remove-at))
         (prefix-in fast- (only-in "fast.rkt" list-set remove-at)))

(define (symbolic-list len)
  (define lst (build-list len identity))
  (apply choose* (for/list ([i len]) (take lst i))))

(define (run-list-set list-set [max-len 20])
  (for ([len (in-range 2 (add1 max-len))])
    (parameterize ([term-cache (hash-copy (term-cache))])
      (clear-asserts!)
      (define-symbolic* idx val integer?)
      (list-set (symbolic-list len) idx val))))

(define (run-remove-at remove-at [max-len 20])
  (for ([len (in-range 2 (add1 max-len))])
    (parameterize ([term-cache (hash-copy (term-cache))])
      (clear-asserts!)
      (define-symbolic* idx integer?)
      (remove-at (symbolic-list len) idx))))

#|
> (time (run-list-set fast-list-set 20))
cpu time: 137 real time: 138 gc time: 36
> (time (run-list-set slow-list-set 20))
cpu time: 1324 real time: 1334 gc time: 300

> (time (run-remove-at fast-remove-at 20))
cpu time: 223 real time: 223 gc time: 53
> (time (run-remove-at slow-remove-at 20))
cpu time: 1255 real time: 1265 gc time: 300
|#