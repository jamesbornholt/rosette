#lang racket

(require "../record.rkt" "../feature.rkt" "key.rkt" "srcloc.rkt"
         racket/date json racket/runtime-path racket/hash)
(provide html-renderer render-json)

; Source of the HTML template
(define-runtime-path template-dir "html")

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
      (for ([n (list "profile.html" "timeline.html" "css" "js")])
        (make-file-or-directory-link (build-path src n) (build-path output-dir n))))

    ; write the JSON data into data.json
    (let ([out (open-output-file (build-path output-dir "data.json"))])
      (render-json profile source name out)
      (close-output-port out))

    ; write the config
    (let ([out (open-output-file (build-path output-dir "config.json"))])
      (fprintf out "Data.config.stream = false;\n")
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
        (system (format "~a ~a" opener (path->string (build-path output-dir "profile.html"))))
        (system (format "~a ~a" opener (path->string (build-path output-dir "timeline.html"))))))))


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


; Transform the profile tree to a list of nodes and list of edges
(define (render-graph profile [key profile-node-key/srcloc])
  (define nodes '())
  (define add-node!
    (let ([i 0])
      (lambda (n)
        (begin0
          i
          (set! nodes (cons n nodes))
          (set! i (add1 i))))))
  (define edges '())
  (define (add-edge! a b)
    (set! edges (cons (list a b) edges)))
    
  (let rec ([node profile][parent -1])
    (let ([proc (key node)][idx (add-node! node)])
      (unless (= parent -1)
        (add-edge! parent idx))
      (for ([c (profile-node-children node)])
        (rec c idx))))

  (values (reverse nodes) (reverse edges)))


; Render entries to JavaScript
; @parameter entries profile-node?
; @parameter source (or/c syntax? #f)
; @parameter out output-port?
(define (render-json profile source name out [key profile-node-key/srcloc])
  (define-values (nodes edges) (render-graph profile key))
  (define graph
    (hash 'nodes (for/list ([n nodes]) (render-entry (key n) n))
          'edges edges))
  (define top-dict
    (hash 'name name
          'time (parameterize ([date-display-format 'rfc2822])
                  (date->string (current-date) #t))
          'source (syntax-srcloc source)
          'form (if (syntax? source) (~v (syntax->datum source)) "")))
  (fprintf out "Data.metadata = ")
  (write-json top-dict out)
  (fprintf out ";\nData.data = ")
  (write-json graph out)
  (fprintf out ";\n"))
