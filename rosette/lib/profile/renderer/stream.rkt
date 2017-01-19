#lang racket

(require "../record.rkt" "key.rkt" "srcloc.rkt" "renderer.rkt"
         racket/date json racket/runtime-path racket/hash)
(provide make-stream-renderer)


; The stream renderer produces a directory containing a webpage version of a profile
; that is updated via a streaming websocket.
(define (make-stream-renderer source name [options (hash)] [key profile-node-key/srcloc])
  (stream-renderer source name key #f))

(struct stream-renderer (source name key [thd #:mutable])
  #:transparent
  #:methods gen:renderer
  [(define (start-renderer self profile reporter)
     (set-stream-renderer-thd! self (stream-renderer-thread profile reporter)))
   (define (finish-renderer self profile)
     (match-define (stream-renderer source name key thd) self)
     (printf "killing thread\n")
     (kill-thread thd))])


(define (stream-renderer-thread profile reporter)
  (thread
   (thunk
    (let loop ()
      (sleep 2)
      (printf "stream-renderer-thread\n")
      (loop)))))
