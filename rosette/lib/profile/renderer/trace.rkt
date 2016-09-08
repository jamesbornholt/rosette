#lang racket

(require "../record.rkt" "key.rkt")

(provide trace-renderer)

; Create a renderer that outputs the entire call tree
(define (trace-renderer)
  (lambda (profile)
    (unless (profile-node? profile)
      (raise-argument-error 'trace-renderer "profile-node?" profile))
    (render-trace profile)))

(define (render-trace profile [key profile-node-key/srcloc])
  (define (indent n)
    (string-join (for/list ([i n]) "  ") ""))
  (let rec ([node profile][level 0])
    (define metrics (profile-node-metrics node))
    (printf "~a* ~a (~v ms, ~v merges, ~v unions, ~v terms)\n"
            (indent level) (key node)
            (hash-ref metrics 'real #f)
            (hash-ref metrics 'merge-count #f)
            (hash-ref metrics 'union-count #f)
            (hash-ref metrics 'term-count #f))
    (for ([c (reverse (profile-node-children node))])
      (rec c (add1 level)))))
