#lang rosette

(define (main x)
  (if (< x 2)
      (if (< 2 x)
          (let ()
            (sleep 1) ; infeasible sleep
            1)
          (let ()
            (sleep 1) ; feasible sleep
            2))
      3))

(define-symbolic x integer?)

;; There should be only ~1 second of infeasible time, not ~2.
(main x)
