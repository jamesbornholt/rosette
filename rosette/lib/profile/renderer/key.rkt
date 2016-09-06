#lang racket

(require "../record.rkt")

(provide (all-defined-out))

(define (profile-node-key/srcloc node)
  (let ([proc (profile-node-procedure node)])
    (if (hash-has-key? (current-sources) proc)
        (format "~a ~v" (object-name proc) (hash-ref (current-sources) proc))
        (~a (object-name proc)))))
