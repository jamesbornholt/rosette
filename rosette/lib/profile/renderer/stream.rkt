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
                                        (hash-ref options 'interval 2.0)))
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

(struct stream-renderer-options (threshold interval) #:transparent)

; The streaming data thread reads data from the profile-state, and sends that
; data to registered client threads.
(define (streaming-data-thread renderer profile reporter)
  (thread
   (thunk
    (match-define (stream-renderer _ _ opts _) renderer)
    (match-define (stream-renderer-options threshold interval) opts)
    
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


(define (make-streaming-client parent renderer profile reporter)
  (match-define (stream-renderer _ _ opts _) renderer)
  (match-define (stream-renderer-options threshold interval) opts)
  (define events-box (profile-state-events profile))
  (lambda (conn state)
    ; register with the streaming data thread
    (printf "Streaming client connected.\n")
    (thread-send parent (current-thread))
    (define res (thread-receive))
    (when (eq? res 'go)
      ; wait for messages
      (let loop ()
        (define sync-result (sync/timeout/enable-break interval (thread-receive-evt)))
        ; get the current events
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
        ; compute the json
        (define msg (jsexpr->bytes (hash 'events (map render-event filtered-events))))
        ; send on the connection
        (with-handlers ([exn:fail? void])
          (ws-send! conn msg)
          ; loop if we should
          (unless sync-result
            (loop)))))
    ; close connection and go away
    (ws-close! conn)))





#|
      (match (sync/enable-break (thread-receive-evt))

    ; spawn the profiler page
    (create-profile-directory renderer)

    (define (get-next-sample-evt)
      (alarm-evt (+ (current-inexact-milliseconds) (* interval 1000))))
    (define thread-rec-evt (thread-receive-evt))
    (define events-box (profile-state-events profile))

    (define total-evts 0)

    (define (pump-updates)
      ; get the current events
      (define events
        (reverse
          ;(cons (profile-event-sample (get-current-metrics #:reporter reporter))
                (let loop ()
                  (define evts (unbox events-box))
                  (if (box-cas! events-box evts '())
                      evts
                      (loop)))));)
      (define filtered-events
        (if (null? events) events (filter-events events threshold)))
      ; compute the json
      (define msg (jsexpr->bytes (hash 'events (map render-event filtered-events))))
      (set! total-evts (+ total-evts (length filtered-events)))
      (printf "events: ~v\n" total-evts)
      ;(printf "first 10: ~v\n" (take filtered-events (min 10 (length filtered-events))))

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
|#

(define (create-profile-directory renderer
                                  #:directory [dir (build-path (current-directory) "profiles")])
  ; get metadata
  (match-define (stream-renderer source name _ _) renderer)
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
