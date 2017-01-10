#lang racket

(require "../record.rkt" "../feature.rkt" "key.rkt" "srcloc.rkt"
         racket/date json racket/runtime-path racket/hash)
(provide html-renderer render-json)

; Source of the HTML template
(define-runtime-path template-dir "html")

; JSON helpers
(define (*->symbol x)
  (if (symbol? x) x (string->symbol (~a x))))
(define (*->string x)
  (if (string? x) x (~a x)))

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


(struct entry-renderer (functions locations inputs outputs metrics start finish) #:mutable
  #:property prop:procedure
  (lambda (self proc node)
    (define d (profile-node-data node))
    (when (false? (entry-renderer-inputs self))
      (define (mapping h)
        (hash-keys h))
      (set-entry-renderer-functions! self (make-hash))
      (set-entry-renderer-locations! self (make-hash))
      (set-entry-renderer-inputs! self (mapping (profile-data-inputs d)))
      (set-entry-renderer-outputs! self (mapping (profile-data-outputs d)))
      (set-entry-renderer-metrics! self (mapping (profile-data-metrics d)))
      (set-entry-renderer-start! self (mapping (profile-data-start d)))
      (set-entry-renderer-finish! self (mapping (profile-data-finish d))))
    (match-define (entry-renderer functions locations inputs outputs metrics start finish) self)
    (define (render data map)
      (for/list ([k map]) (hash-ref data k 0)))
    (list (hash-ref! functions proc (thunk (hash-count functions)))
          (hash-ref! locations (syntax-srcloc (profile-data-location d)) (thunk (hash-count locations)))
          (render (profile-data-inputs d) inputs)
          (render (profile-data-outputs d) outputs)
          (render (profile-data-metrics d) metrics)
          (render (profile-data-start d) start)
          (render (profile-data-finish d) finish))))
          
          


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
(define (render-graph out profile [key profile-node-key/srcloc])
  (define (dt node)
    (- (hash-ref (profile-data-finish (profile-node-data node)) 'time)
       (hash-ref (profile-data-start  (profile-node-data node)) 'time)))
  (define MIN_TIME (* (dt profile) 0.001))
  (define (include? node)
    (> (dt node) MIN_TIME))
                
  (fprintf out "{'nodes': [")

  (define renderer (entry-renderer #f #f #f #f #f #f #f))
  (define add-node!
    (let ([i 0])
      (lambda (node)
        (begin0
          i
          (set! i (+ i 1))
          (write-json (renderer (key node) node) out)
          (fprintf out ", ")))))
  (define edges '())
  (define (add-edge! a b)
    (set! edges (cons (list a b) edges)))
    
  (let rec ([node profile][parent -1])
    (when (include? node)
      (let ([idx (add-node! node)])
        (unless (= parent -1)
          (add-edge! parent idx))
        (for ([c (profile-node-children node)])
          (rec c idx)))))

  (fprintf out "{}], 'edges': ")
  (write-json (reverse edges) out)
  (fprintf out ", 'map': ")

  (define (reverse-map m)
    (for/list ([k m])
      (*->string (if (feature? k) (feature-name k) k))))
  (match-define (entry-renderer functions locations inputs outputs metrics start finish) renderer)
  (write-json (hash 'top (list "function" "location" "inputs" "outputs" "metrics" "start" "finish")
                    'function (let ([pairs (hash->list functions)]) (map car (sort pairs < #:key cdr)))
                    'location (let ([pairs (hash->list locations)]) (map car (sort pairs < #:key cdr)))
                    'inputs (reverse-map inputs)
                    'outputs (reverse-map outputs)
                    'metrics (reverse-map metrics)
                    'start (reverse-map start)
                    'finish (reverse-map finish))
              out)
  (fprintf out "}"))


; Render entries to JavaScript
; @parameter entries profile-node?
; @parameter source (or/c syntax? #f)
; @parameter out output-port?
(define (render-json profile source name out [key profile-node-key/srcloc])
  (fprintf out "Data.data = ")
  (render-graph out profile key)
  (fprintf out ";\n")
  (define top-dict
    (hash 'name name
          'time (parameterize ([date-display-format 'rfc2822])
                  (date->string (current-date) #t))
          'source (syntax-srcloc source)
          'form (if (syntax? source) (~v (syntax->datum source)) "")))
  (fprintf out "Data.metadata = ")
  (write-json top-dict out)
  (fprintf out ";\n"))
