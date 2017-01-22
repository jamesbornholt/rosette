#lang racket

(require "../../record.rkt" "../../graph.rkt")

(provide (all-defined-out))

(define (profile-node-key/srcloc node)
  (procedure-name (profile-data-procedure (profile-node-data node))))

(define (profile-node-key/callloc node)
  (let ([data (profile-node-data node)])
    (format "~a @ ~a" (object-name (profile-data-procedure data)) (profile-data-location data))))

(define (procedure-name proc)
  (if (hash-has-key? (current-sources) proc)
      (format "~a ~v" (object-name proc) (hash-ref (current-sources) proc))
      (~a (object-name proc))))