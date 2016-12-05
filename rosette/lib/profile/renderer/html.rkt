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

    ; set up output directory
    (define output-dir (build-path dir (make-folder-name source)))
    (make-directory* output-dir)

    ; link the template files into the output directory
    (let ([src (path->complete-path template-dir)])
      (for ([n (list "index.html" "css" "js")])
        (make-file-or-directory-link (build-path src n) (build-path output-dir n))))

    ; write the JSON data into data.json
    (let ([out (open-output-file (build-path output-dir "data.json"))])
      (render-json profile source name out)
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


; Render a single profile-node? to a jsexpr? dictionary
(define (render-entry proc node)
  (define (convert h)
    (for/hash ([(k v) h])
      (values (*->symbol (if (feature? k) (feature-name k) k)) v)))
  (define metrics-excl
    (for/hash ([(k v) (profile-data-metrics (profile-node-data node))])
      (values (string->symbol (format "~a (excl.)" k))
              (- v (for/sum ([c (profile-node-children node)])
                     (hash-ref (profile-data-metrics (profile-node-data c)) k 0))))))
  (hash 'inputs (convert (profile-data-inputs (profile-node-data node)))
        'outputs (convert (profile-data-outputs (profile-node-data node)))
        'metrics (hash-union (convert (profile-data-metrics (profile-node-data node))) metrics-excl)
        'start (convert (profile-data-start (profile-node-data node)))
        'finish (convert (profile-data-finish (profile-node-data node)))
        'function proc
        'location (syntax-srcloc (profile-data-location (profile-node-data node)))))


; Render entries to JavaScript
; @parameter entries profile-node?
; @parameter source (or/c syntax? #f)
; @parameter out output-port?
(define (render-json profile source name out [key profile-node-key/srcloc])
  (define graph
    (let rec ([node profile])
      (let ([proc (key node)])
        (define entry (render-entry proc node))
        (hash-set entry 'children (reverse (for/list ([c (profile-node-children node)]) (rec c)))))))
  (define top-dict
    (hash 'name name
          'time (parameterize ([date-display-format 'rfc2822])
                  (date->string (current-date) #t))
          'source (syntax-srcloc source)
          'form (if (syntax? source) (~v (syntax->datum source)) "")))
  (fprintf out "Profile.data = ")
  (write-json top-dict out)
  (fprintf out ";\nProfile.graph = ")
  (write-json graph out)
  (fprintf out ";\n"))
