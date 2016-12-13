#lang racket

(require "record.rkt" "renderer/html.rkt" "renderer/srcloc.rkt"
         racket/runtime-path)
(provide profile-stream)


(define (profile-stream-thunk thunk #:source [source-stx #f]
                                    #:name [name "Profile"])
  (define-values (stream-path out-path) (begin-stream-renderer source-stx))
  (define profile (make-top-level-profile))
  (set-profile-node-data! profile (entry-data 'top 'top '()))
  (define thd (stream-thread 2.0 stream-path profile source-stx name))
  (define ret (parameterize ([current-profile profile])
                (define-values (out cpu real gc) (time-apply thunk '()))
                (record-exit! out cpu real gc)
                out))
  (kill-thread thd)
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

  (values (build-path output-dir "stream.json")
          (build-path output-dir "data.json")))


(define (stream-thread delay path profile source name)
  (define (loop)
    (sleep delay)
    (define out (open-output-file path #:exists 'replace))
    (render-json profile source name out)
    (close-output-port out)
    (loop))
  (thread loop))


; Profile the given form
(define-syntax (profile-stream stx)
  (syntax-case stx ()
    [(_ expr args ...)
     (syntax/loc stx
       (profile-stream-thunk (thunk expr) #:source #'expr args ...))]))
