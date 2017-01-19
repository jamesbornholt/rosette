#lang racket

(require "../record.rkt" "key.rkt" "srcloc.rkt" "renderer.rkt"
         (only-in "html.rkt" render-entry)
         net/rfc6455
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
     (set-stream-renderer-thd! self (streaming-data-thread profile reporter (stream-renderer-key self) 2.0)))
   (define (finish-renderer self profile)
     (match-define (stream-renderer source name key thd) self)
     (printf "done: ~v\n" (hash-count (profile-stream-node->idx (profile-state-stream profile))))
     (thread-send thd 'done)
     (printf "waiting for thread...\n")
     (thread-wait thd)
     (printf "done!\n"))])

; The streaming data thread reads data from the profile-state, and sends that
; data to registered client threads.
(define (streaming-data-thread profile reporter key interval)
  (thread
   (thunk
    (define clients '())

    (define messages '())
    
    (define server-shutdown! (ws-serve #:port 8081 (make-streaming-client (current-thread))))

    (define (get-next-sample-evt)
      (alarm-evt (+ (current-inexact-milliseconds) (* interval 1000))))
    (define thread-rec-evt (thread-receive-evt))
    
    (define stream-data (profile-state-stream profile))

    (define (pump-updates)
      ; lock the stream data so we see an atomic view of it and don't clobber updates
      (semaphore-wait (profile-stream-lock stream-data))

      ; read all updates and clear them out
      (define new-nodes (profile-stream-nodes stream-data))
      (define new-edges (profile-stream-edges stream-data))
      (define new-finished (profile-stream-finished stream-data))
      (set-profile-stream-nodes! stream-data '())
      (set-profile-stream-edges! stream-data '())
      (set-profile-stream-finished! stream-data '())

      ; release the lock on the stream data
      (semaphore-post (profile-stream-lock stream-data))

      ; the new message to broadcast
      (define msg (make-stream-message new-nodes new-edges new-finished key))

      ; send message to all threads
      (for ([t clients])
        (thread-send t msg))

      ; hold onto this message for newly connected clients
      (set! messages (cons msg messages)))
    
    (let loop ([sample-evt (get-next-sample-evt)])
      (printf "streaming-data-thread is waiting...\n")
      (define evt (sync/enable-break thread-rec-evt sample-evt))
      (printf "streaming-data-thread got msg ~v\n" evt)
      (when (equal? evt thread-rec-evt)
        (set! evt (thread-receive)))
      (match evt
        [(and t (? thread?))
         (set! clients (cons t clients))
         (for ([msg (reverse messages)])
           (thread-send t msg))
         (loop sample-evt)]
        ['done
         (pump-updates)
         (for ([t clients])
           (thread-send t 'done)
           (thread-wait t))
         (server-shutdown!)]
        [_
         (pump-updates)
         (loop (get-next-sample-evt))])))))


(define (make-stream-message nodes edges finished key)
  (jsexpr->bytes
   (hash 'nodes (for/list ([n (reverse nodes)]) (render-entry (key n) n))
         'edges (reverse edges)
         'finished (reverse finished))))
  
  
(define (make-streaming-client parent)
  (lambda (conn state)
    ; register with the streaming data thread
    (thread-send parent (current-thread))
    (printf "streaming client is waiting...\n")
    ; wait for messages
    (let loop ()
      (match (thread-receive)
        ['done  ; wrap up
         (printf "streaming client is going away\n")
         (ws-close! conn)]
        [msg  ; send message to client
         (printf "streaming client got a message\n")
         (ws-send! conn msg)
         (loop)]))))

#|
(define (stream-renderer-thread profile reporter)
  (thread
   (thunk
    (define shutdown-server #f)
    (define done-chan (make-channel))
    (define (serve conn state)
      (define sum 0)
      (let loop ()
        (define evt (sync/timeout/enable-break 2 done-chan))
        (define stream-data (profile-state-stream profile))
        (define finished (profile-stream-finished stream-data))
        (set-profile-stream-finished! stream-data '())
        (define len (length finished))
        (set! sum (+ sum len))
        (printf "stream-renderer-thread... ~v (~v)\n" len sum)
        (ws-send! conn (format "finished ~v (~v)" len sum))
        (match evt
          [#f (loop)]
          [_ (channel-put done-chan 'done)])))
    (set! shutdown-server (ws-serve #:port 8081 serve))
    (match (thread-receive)
      ['done (channel-put done-chan 'done)  ; tell thread to shut down
             (channel-get done-chan)  ; wait until it's done
             (shutdown-server)]))))  ; shutdown the websocket
|#