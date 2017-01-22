#lang racket

(require "../record.rkt"
         "renderer.rkt"
         "util/key.rkt" "util/srcloc.rkt"
         (only-in "html.rkt" compute-graph render-entry)
         net/rfc6455 net/sendurl
         racket/date json racket/runtime-path racket/hash)
(provide make-stream-renderer)


; Source of the HTML template
(define-runtime-path template-dir "html")


; The stream renderer produces a directory containing a webpage version of a profile
; that is updated via a streaming websocket.
(define (make-stream-renderer source name [options (hash)] [key profile-node-key/srcloc])
  (stream-renderer source name key 2.0 #f))

(struct stream-renderer (source name key interval [thd #:mutable])
  #:transparent )#|
  #:methods gen:renderer
  [(define (start-renderer self profile reporter)
     (set-stream-renderer-thd!
      self
      (streaming-data-thread self profile reporter)))
   (define (finish-renderer self profile)
     (match-define (stream-renderer _ _ _ _ thd) self)
     (printf "done\n")
     (thread-send thd 'done)
     (printf "waiting for thread...\n")
     (thread-wait thd)
     (printf "done!\n"))])


; The streaming data thread reads data from the profile-state, and sends that
; data to registered client threads.
(define (streaming-data-thread renderer profile reporter)
  (thread
   (thunk
    (match-define (stream-renderer _ _ key interval _) renderer)
    
    (define clients '())
    
    (define server-shutdown! (ws-serve #:port 8081 (make-streaming-client (current-thread))))

    ; spawn the profiler page
    (create-profile-directory renderer)

    (define (get-next-sample-evt)
      (alarm-evt (+ (current-inexact-milliseconds) (* interval 1000))))
    (define thread-rec-evt (thread-receive-evt))

    (define (pump-updates)
      ; get the current profile
      (define-values (nodes edges) (compute-graph (profile-state-root profile) (stream-renderer-key renderer)))
      ; compute the json
      (define msg
        (jsexpr->bytes
         (hash 'nodes (for/list ([n nodes]) (render-entry (key n) n))
               'edges edges)))

      ; send message to all threads
      (for ([t clients] #:when (thread-running? t))
        (thread-send t msg #f)))
    
    (let loop ([sample-evt (get-next-sample-evt)])
      (printf "streaming-data-thread is waiting...\n")
      (define evt (sync/enable-break thread-rec-evt sample-evt))
      (printf "streaming-data-thread got msg ~v\n" evt)
      (when (equal? evt thread-rec-evt)
        (set! evt (thread-receive)))
      (match evt
        [(and t (? thread?))
         (set! clients (cons t clients))
         (loop sample-evt)]
        ['done
         (pump-updates)
         (for ([t clients] #:when (thread-running? t))
           (thread-send t 'done #f)
           (thread-wait t))
         (server-shutdown!)]
        [_
         (pump-updates)
         (loop (get-next-sample-evt))])))))


(define (create-profile-directory renderer
                                  #:directory [dir (build-path (current-directory) "profiles")])
  ; get metadata
  (match-define (stream-renderer source name _ _ _) renderer)
  (define top-dict
    (hash 'name name
          'time (parameterize ([date-display-format 'rfc2822])
                  (date->string (current-date) #t))
          'source (syntax-srcloc source)
          'form (if (syntax? source) (~v (syntax->datum source)) "")))
  
  ; set up output directory
  (define output-dir (build-path dir (make-folder-name source)))
  (make-directory* output-dir)

  ; link the template files into the output directory
  (let ([src (path->complete-path template-dir)])
    (for ([n (list "profile.html" "timeline.html" "css" "js")])
      (make-file-or-directory-link (build-path src n) (build-path output-dir n))))

    ; write the config
  (let ([out (open-output-file (build-path output-dir "config.json"))])
    (fprintf out "Data.config.stream = true;\n")
    (fprintf out "Data.metadata = ")
    (write-json top-dict out)
    (fprintf out ";\n")
    (close-output-port out))
  
  ; open the profile in a web browser
  (printf "Wrote \"~a\" profile to ~a\n" name output-dir)
  (send-url/file (build-path output-dir "timeline.html")))

  
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
         (with-handlers ([exn:fail? (lambda (e) (ws-close! conn))])
           (ws-send! conn msg)
           (loop))]))))

|#

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