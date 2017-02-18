#lang rosette

(define (main x)
  (if (< x 2)
      (if (< 2 x)
          (let ()
            (sleep 1)
            1)
          2)
      3))

(define-symbolic x integer?)

(main x)
