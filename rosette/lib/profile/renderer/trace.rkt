#lang racket

(require "../data.rkt" "../record.rkt" "../graph.rkt" "../reporter.rkt"
         "renderer.rkt"
         "util/key.rkt")

(provide make-trace-renderer)

(struct trace-renderer (source name key) 
  #:transparent
  #:methods gen:renderer
  [(define start-renderer void)
   (define done-running void)
   (define (finish-renderer self profile)
     (match-define (trace-renderer source name key) self)
     (render-trace (profile-state->graph profile) source name key))])

(define (make-trace-renderer source name [options (hash)] [key profile-node-key/srcloc])
  (trace-renderer source name key))

(define (render-trace profile source name [key profile-node-key/srcloc])
  (define (indent n)
    (string-join (for/list ([i n]) "  ") ""))
  (printf "Trace for ~a (~v)\n" name source)
  (let rec ([node profile][level 0])
    (define metrics (profile-data-metrics (profile-node-data node)))
    (printf "~a* ~a (~v ms, ~v merges, ~v unions, ~v terms)\n"
            (indent level) (key node)
            (metrics-real metrics)
            (metrics-merge-count metrics)
            (metrics-union-count metrics)
            (metrics-term-count metrics))
    (for ([c (reverse (profile-node-children node))])
      (rec c (add1 level)))))
