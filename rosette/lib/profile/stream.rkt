#lang racket

(require rosette/base/core/reporter
         "record.rkt" "reporter.rkt" "tool.rkt"
         "renderer/html.rkt" "renderer/srcloc.rkt"
         racket/runtime-path json)
(provide profile-stream)

(define profile-stream void)

#|
(define (profile-stream-thunk thunk #:source [source-stx #f]
                                    #:name [name "Profile"])
  (define-values (stream-path out-path) (begin-stream-renderer source-stx))
  (define profile (make-top-level-profile))
  (define reporter (make-profiler-reporter))
  (set-profile-node-data! profile (entry-data 'top 'top '()))
  (define thd (stream-thread 2.0 stream-path profile reporter source-stx name))
  (define ret (parameterize ([current-profile profile]
                             [current-reporter reporter])
                (define-values (out cpu real gc) (time-apply thunk '()))
                (record-exit! out cpu real gc)
                out))
  (thread-send thd 'stop) ; tell the streaming thread to write once more then quit
  (thread-wait thd)
  (let ([out (open-output-file out-path)])
    (render-json profile source-stx name out)
    (close-output-port out))
  (apply values ret))


; Source of the HTML template
(define-runtime-path template-dir "renderer/html")

; Initialize the output directory for the streamed profile
; Returns the filename for the streaming output.
(define (begin-stream-renderer source
                               #:directory [dir (build-path (current-directory) "profiles")])
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
    (close-output-port out))

  ; touch the streaming output file
  (write-to-file "" (build-path output-dir "stream.json"))

  ; open the timeline page after a delay *TODO HACK*
  (thread
   (thunk
    (sleep 4)
    (define opener
      (match (system-type)
        ['unix "xdg-open"]
        ['windows "start"]
        ['macosx "open"]
        [_ #f]))
    (unless (false? opener)
      (printf "Opening streaming timeline...\n")
      (system (format "~a ~a" opener (path->string (build-path output-dir "timeline.html")))))))

  (values (build-path output-dir "stream.json")
          (build-path output-dir "data.json")))


; The stream thread does two things:
; 1. every `delay` seconds, writes all current profile data to `path`
; 2. every `sample-delay` seconds, takes a sample of current state
(define (stream-thread delay path profile reporter source name [sample-delay 0.5])
  (thread
   (thunk
    (define last-out 0)
    (define samples '())
    (let loop ()
      (define ret (sync/timeout (min delay sample-delay) (thread-receive-evt)))
      (when (or ret (> (- (current-inexact-milliseconds) last-out) (* delay 1000)))
        (set! last-out (current-inexact-milliseconds))
        (call-with-atomic-output-file
         path
         (lambda (out path)
           (render-json profile source name out)
           (fprintf out "Data.samples = ")
           (write-json (reverse samples) out)
           (fprintf out ";\n"))))
      (set! samples (cons (get-current-metrics reporter) samples))
      (unless ret (loop))))))  ; loop unless we received a 'stop message


; Profile the given form
(define-syntax (profile-stream stx)
  (syntax-case stx ()
    [(_ expr args ...)
     (syntax/loc stx
       (profile-stream-thunk (thunk expr) #:source #'expr args ...))]))
|#