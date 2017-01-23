#lang racket

(require "../record.rkt" "../feature.rkt" "../graph.rkt" "../reporter.rkt"
         "renderer.rkt"
         "util/key.rkt" "util/srcloc.rkt"
         racket/date json racket/runtime-path racket/hash net/sendurl)
(provide make-html-renderer filter-events render-event)

; Source of the HTML template
(define-runtime-path template-dir "html")

; JSON helpers
(define (*->symbol x)
  (if (symbol? x) x (string->symbol (~a x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The HTML renderer produces a directory containing a webpage version of a profile.
(define (make-html-renderer source name [options (hash)] [key profile-node-key/srcloc])
  (define opts (html-renderer-options (hash-ref options 'html-profile #f)
                                      (hash-ref options 'threshold 0.001)))
  (html-renderer source name key opts))

(struct html-renderer (source name key opts) 
  #:transparent
  #:methods gen:renderer
  [(define start-renderer void)
   (define (finish-renderer self profile)
     (match-define (html-renderer source name key opts) self)
     (render-html profile source name opts))])

(struct html-renderer-options (profile? threshold) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (render-html profile source name opts
                     #:directory [dir (build-path (current-directory) "profiles")])
  ; set up output directory
  (define output-dir (build-path dir (make-folder-name source)))
  (make-directory* output-dir)

  ; link the template files into the output directory
  (let ([src (path->complete-path template-dir)])
    (for ([n (list "profile.html" "timeline.html" "css" "js")])
      (make-file-or-directory-link (build-path src n) (build-path output-dir n))))

  ; write the JSON data into data.json
  (let ([out (open-output-file (build-path output-dir "data.json"))])
    (render-json profile source name out opts)
    (close-output-port out))

  ; write the config
  (let ([out (open-output-file (build-path output-dir "config.json"))])
    (fprintf out "Data.config.stream = false;\n")
    (close-output-port out))

  ; open the profiles in a web browser
  (printf "Wrote \"~a\" profile to ~a\n" name output-dir)
  (when (html-renderer-options-profile? opts)
    (send-url/file (build-path output-dir "profile.html")))
  (send-url/file (build-path output-dir "timeline.html")))


;; Filter a stream of profile-event records to not include event pairs whose
;; duration is longer than min% percent of the total execution time.
;; Any unmatched pairs (created when the event stream is incomplete) will be
;; passed through unmodified.
(define (filter-events events [min% 0.001])
  ; determine the minimum time for an event to be included
  (define (event->time evt)
    (match evt
      [(profile-event-enter _ _ _ m) (metrics-time m)]
      [(profile-event-exit _ m) (metrics-time m)]))
  (define (dt enter exit)
    (- (event->time exit)
       (event->time enter)))
  (define MIN_TIME (* (dt (first events) (last events)) min%))

  ; filter events by building a stack and event list in parallel.
  ; whenever we pop from the stack, we check that the dt for the current node
  ; is above the threshold. if not, we delete everything from the list between
  ; the head and the index stored when the current node was pushed.
  (define new-events '())
  (define stack '())
  (define n 0)
  (for ([e events])
    (cond [(profile-event-enter? e)
           (set! stack (cons (cons e n) stack))
           (set! new-events (cons e new-events))
           (set! n (add1 n))]  ; n tracks the length of new-events
          [(and (profile-event-exit? e) (null? stack))  ; allow unmatched (for streaming)
           (set! new-events (cons e new-events))
           (set! n (add1 n))]
          [(profile-event-exit? e)
           (match-define (cons enter k) (car stack))
           (cond [(> (dt enter e) MIN_TIME)  ; allow the event
                  (set! new-events (cons e new-events))
                  (set! n (add1 n))]
                 [else  ; filter the event and its entire stack frame
                  (define del (- n k))
                  (set! new-events (drop new-events del))
                  (set! n k)])
           (set! stack (cdr stack))]
          [else
           (set! new-events (cons e new-events))
           (set! n (add1 n))]))
  ; include any unmatched events for streaming purposes
  (unless (null? stack)
    (set! new-events (append (reverse stack) new-events)))

  (reverse new-events))


;; Render an event to a jsexpr? dictionary
(define (render-event event [key procedure-name])
  (match event
    [(profile-event-enter loc proc in met)
     (hash 'type "ENTER"
           'function (key proc)
           'location (syntax-srcloc loc)
           'inputs (profile-hash->jsexpr in)
           'metrics (metrics->jsexpr met))]
    [(profile-event-exit out met)
     (hash 'type "EXIT"
           'outputs (profile-hash->jsexpr out)
           'metrics (metrics->jsexpr met))]
    [_ (error 'render-event "unknown event ~v" event)]))


;; Helper to convert a hash to a jsexpr?, including converting features to
;; their names.
(define (profile-hash->jsexpr h)
  (for/hash ([(k v) h])
    (values (*->symbol (if (feature? k) (feature-name k) k)) v)))


;; Convert an instance of metrics? to a hash for output
(define (metrics->jsexpr m)
  (match-define (metrics term-count merge-count union-count union-size cpu real gc time) m)
  (hash 'term-count term-count 'merge-count merge-count
        'union-count union-count 'union-size union-size
        ; 'cpu cpu 'real real 'gc gc 
        'time time))


; Render entries to JavaScript
; @parameter entries profile-node?
; @parameter source (or/c syntax? #f)
; @parameter out output-port?
(define (render-json state source name out opts [key profile-node-key/srcloc])
  (define metadata
    (hash 'name name
          'time (parameterize ([date-display-format 'rfc2822])
                  (date->string (current-date) #t))
          'source (syntax-srcloc source)
          'form (if (syntax? source) (~v (syntax->datum source)) "")))
  (fprintf out "Data.metadata = ")
  (write-json metadata out)
  (fprintf out ";\n")
  (define events (reverse (unbox (profile-state-events state))))
  (define filtered-events (filter-events events (html-renderer-options-threshold opts)))
  (fprintf out "Data.events = ")
  (write-json (map render-event filtered-events) out)
  (fprintf out ";\n"))
