#lang racket

(require "../record.rkt" "../reporter.rkt"
         "renderer.rkt"
         "util/key.rkt" "util/srcloc.rkt"
         (only-in "html.rkt" filter-events render-event)
         net/rfc6455 net/sendurl
         racket/date json racket/runtime-path racket/hash)
(provide make-stream-renderer)


; Source of the HTML template
(define-runtime-path template-dir "html")


; The stream renderer produces a directory containing a webpage version of a profile
; that is updated via a streaming websocket.
(define (make-stream-renderer source name [options (hash)] [key profile-node-key/srcloc])
  (define opts (stream-renderer-options (hash-ref options 'threshold 0.001)
                                        (hash-ref options 'interval 2.0)
                                        (hash-ref options 'symlink #f)))
  (stream-renderer source name opts #f))

(struct stream-renderer (source name opts [thd #:mutable])
  #:transparent
  #:methods gen:renderer
  [(define (start-renderer self profile reporter)
     (set-stream-renderer-thd!
      self
      (streaming-data-thread self profile reporter)))
   (define (finish-renderer self profile)
     (match-define (stream-renderer _ _ _ thd) self)
     (thread-send thd 'done)
     (thread-wait thd))])

(struct stream-renderer-options (threshold interval symlink?) #:transparent)


; The streaming data thread reads data from the profile-state, and sends that
; data to registered client threads.
(define (streaming-data-thread renderer profile reporter)
  (thread
   (thunk
    (match-define (stream-renderer _ _ opts _) renderer)
    (match-define (stream-renderer-options threshold interval _) opts)
    
    (define server-shutdown!
      (ws-serve #:port 8081
                (make-streaming-client (current-thread) renderer profile reporter)))

    (create-profile-directory renderer)

    (define client #f)

    (let loop ()
      (sync/enable-break (thread-receive-evt))
      (match (thread-receive)
        [(and t (? thread?))
         ; new client connection... reject it if we already have one
         (if client
             (thread-send t 'quit #f)
             (begin
               (set! client t)
               (thread-send t 'go)))
         (loop)]
        ['done
         (when (and (thread? client) (not (thread-dead? client)))
           (thread-send client 'done #f)
           (thread-wait client))
         (server-shutdown!)])))))


; Create a procedure which can be invoked by the WebSocket server to start
; feeding data to a new client.
; Only one client can be connected at a time. If a second client connects, the
; initial message received from the parent thread will not be 'go, and the
; client will disconnect immediately.
(define (make-streaming-client parent renderer profile reporter)
  (match-define (stream-renderer _ _ opts _) renderer)
  (match-define (stream-renderer-options threshold interval _) opts)
  (define events-box (profile-state-events profile))
  (lambda (conn state)
    ; register with the streaming data thread
    (printf "Streaming client connected.\n")
    (thread-send parent (current-thread))
    (define res (thread-receive))
    (when (eq? res 'go)
      (let loop ()
        ; wait for messages or timeout
        (define sync-result (sync/timeout/enable-break interval (thread-receive-evt)))
        ; always pump new events, even if we got a 'quit message
        (define events
          (reverse
           (cons (profile-event-sample (get-current-metrics #:reporter reporter))
                 (let loop ()
                   (define evts (unbox events-box))
                   (if (box-cas! events-box evts '())
                       evts
                       (loop))))))
        (define filtered-events
          (if (null? events) events (filter-events events threshold)))
        (define msg (jsexpr->bytes (hash 'events (map render-event filtered-events))))
        ; send on the connection. if sending fails, bail out of the loop.
        (with-handlers ([exn:fail? void])
          (ws-send! conn msg)
          (unless sync-result  ; loop if sync-result = #f ==> triggered by timeout
            (loop)))))
    (ws-close! conn)))


; Create the output directory and seed it with the configuration data, then open
; the webpage in a browser.
(define (create-profile-directory renderer)
  (match-define (stream-renderer source name opts _) renderer)
  (match-define (stream-renderer-options _ _ symlink?) opts)
  
  ; get metadata for this profile
  (define top-dict
    (hash 'name name
          'time (parameterize ([date-display-format 'rfc2822])
                  (date->string (current-date) #t))
          'source (syntax-srcloc source)
          'form (if (syntax? source) (~v (syntax->datum source)) "")))
  
  ; set up output directory
  (define output-dir 
    (if symlink?
        (build-path (current-directory) "profiles" (make-folder-name source))
        (build-path (find-system-path 'temp-dir) (make-folder-name source))))
  (make-directory* output-dir)

  ; link the template files into the output directory
  (define copy-or-symlink (if symlink?
                              make-file-or-directory-link
                              copy-directory/files))
  (let ([src (path->complete-path template-dir)])
    (for ([n (list "timeline.html" "css" "js")])
      (copy-or-symlink (build-path src n) (build-path output-dir n))))

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
