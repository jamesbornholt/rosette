#lang racket

(require "../record.rkt" "../feature.rkt" "stats.rkt" "key.rkt"
         racket/date json racket/runtime-path racket/hash)
(provide html-renderer)

; Source of the HTML template
(define-runtime-path template-dir "html")

; Assign an increasing identifier to each invocation of html-renderer
(define file-count 0)

; Helpers to construct filenames
(define datestr
  (match-let ([pad (lambda (x n) (~r x #:min-width n #:pad-string "0"))]
              [(date s m h d M y _ _ _ _) (current-date)])
    (string-append (pad y 4) (pad M 2) (pad d 2) (pad h 2) (pad m 2) (pad s 2))))
(define (syntax-srcfile stx)
  (if (and (syntax? stx) (path? (syntax-source stx)))
      (let-values ([(base name dir?) (split-path (syntax-source stx))])
        (path->string name))
      "unknown"))
(define (basename path)
  (let-values ([(base name dir?) (split-path path)])
    name))
(define (syntax-srcloc stx)
  (cond [(and (syntax? stx) (path? (syntax-source stx)))
         (format "~a:~v:~v" (path->string (basename (syntax-source stx))) (syntax-line stx) (syntax-column stx))]
        [(and (list? stx) (= (length stx) 3))
         (match-let* ([(list src line col) stx]
                      [name (if (path? src) (path->string (basename src)) (~a src))])
           (format "~a:~v:~v" name line col))]
        [else (~a stx)]))
(define (make-folder-name source)
  (begin0
    (format "~a-~a-~v" (syntax-srcfile source) datestr file-count)
    (set! file-count (add1 file-count))))

; JSON helpers
(define (*->symbol x)
  (if (symbol? x) x (string->symbol (~a x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The HTML renderer produces a directory containing a webpage version of a complexity profile.
(define (html-renderer #:directory [dir (build-path (current-directory) "profiles")]
                       #:auto-open? [open? #t])
  (lambda (profile source name)
    (unless (profile-node? profile)
      (raise-argument-error 'html-renderer "profile-node?" profile))

    ; generate profile data
    (define entries (aggregate profile))

    ; set up output directory
    (define output-dir (build-path dir (make-folder-name source)))
    (make-directory* output-dir)

    ; link the template files into the output directory
    (let ([src (path->complete-path template-dir)])
      (for ([n (list "index.html" "css" "js")])
        (make-file-or-directory-link (build-path src n) (build-path output-dir n))))

    ; write the JSON data into data.json
    (let ([out (open-output-file (build-path output-dir "data.json"))])
      (render-json entries source name out)
      (close-output-port out))

    ; open the profile in a web browser
    (printf "Wrote \"~a\" profile to ~a\n" name output-dir)
    (when open?
      (define opener
        (match (system-type)
          ['unix "xdg-open"]
          ['windows "start"]
          ['macosx "open"]
          [_ #f]))
      (unless (false? opener)
        (printf "Opening profile...\n")
        (system (format "~a ~a" opener (path->string (build-path output-dir "index.html"))))))))


; Aggregate profile records by procedure
(define (aggregate profile [key profile-node-key/srcloc])
  (define functions (make-hash))
  (let rec ([node profile])
    (unless (false? (profile-node-procedure node))
      (let ([proc (key node)])
        (hash-update! functions proc (lambda (old) (cons node old)) '())))
    (for ([c (profile-node-children node)])
      (rec c)))
  functions)


; Render a single profile-node? to a jsexpr? dictionary
(define (render-entry node)
  (define (convert h)
    (for/hash ([(k v) h])
      (values (*->symbol (if (feature? k) (feature-name k) k)) v)))
  (define metrics-excl
    (for/hash ([(k v) (profile-node-metrics node)])
      (values (string->symbol (format "~a (excl.)" k))
              (- v (for/sum ([c (profile-node-children node)])
                     (hash-ref (profile-node-metrics c) k 0))))))
  (hash 'inputs (convert (profile-node-inputs node))
        'outputs (convert (profile-node-outputs node))
        'metrics (hash-union (convert (profile-node-metrics node)) metrics-excl)
        'location (syntax-srcloc (profile-node-location node))))


; Render entries to JSON
; @parameter entries (hashof symbol? profile-node?)
; @parameter source (or/c syntax? #f)
; @parameter out output-port?
(define (render-json entries source name out)
  (define dict
    (hash 'name name
          'time (parameterize ([date-display-format 'rfc2822])
                  (date->string (current-date) #t))
          'source (syntax-srcloc source)
          'form (if (syntax? source) (~v (syntax->datum source)) "")
          'functions (for/list ([(proc nodes) entries])
                       (hash 'name proc
                             'calls (for/list ([n nodes]) (render-entry n))))))
  (fprintf out "Profile.data = ")
  (write-json dict out))
