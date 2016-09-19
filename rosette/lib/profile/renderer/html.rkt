#lang racket

(require "../record.rkt" "../feature.rkt" "stats.rkt" "key.rkt"
         racket/date json racket/runtime-path)
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
(define (syntax-srcloc stx)
  (if (and (syntax? stx) (path? (syntax-source stx)))
      (let-values ([(base name dir?) (split-path (syntax-source stx))])
        (format "~a line ~v column ~v" (path->string name) (syntax-line stx) (syntax-column stx)))
      stx))
(define (make-folder-name source)
  (begin0
    (format "~a-~a-~v" (syntax-srcfile source) datestr file-count)
    (set! file-count (add1 file-count))))

; JSON helpers
(define (*->symbol x)
  (if (symbol? x) x (string->symbol (~a x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The HTML renderer produces a directory containing a webpage version of a complexity profile.
(define (html-renderer #:directory [dir (build-path (current-directory) "profiles")])
  (lambda (profile source)
    (unless (profile-node? profile)
      (raise-argument-error 'html-renderer "profile-node?" profile))
    (define entries (aggregate profile))
    (define output-dir (build-path dir (make-folder-name source)))
    (make-directory* dir)
    (copy-directory/files template-dir output-dir)
    (let ([out (open-output-file (build-path output-dir "data.json"))])
      (render-json entries source out)
      (close-output-port out))
    (printf "Wrote profile to ~a\n" output-dir)))


; Aggregate profile records by procedure
(define (aggregate profile [key profile-node-key/srcloc])
  (define functions (make-hash))
  (let rec ([node profile])
    (let ([proc (key node)])
      (hash-update! functions proc (lambda (old) (cons node old)) '()))
    (for ([c (profile-node-children node)])
      (rec c)))
  functions)


; Render entries to JSON
; @parameter entries (hashof symbol? profile-node?)
; @parameter source (or/c syntax? #f)
; @parameter out output-port?
(define (render-json entries source out [key profile-node-key/srcloc])
  ; Render a single entry to a jsexpr? hash
  (define (render-entry node)
    (define (convert h) (for/hash ([(k v) h]) (values (*->symbol (if (feature? k) (feature-name k) k)) v)))
    (hash 'inputs (convert (profile-node-inputs node))
          'outputs (convert (profile-node-outputs node))
          'metrics (convert (profile-node-metrics node))))
  (define dict
    (hash 'source (syntax-srcloc source)
          'form (if (syntax? source) (~v (syntax->datum source)) "")
          'functions (for/list ([(proc nodes) entries])
                       (hash 'name proc
                             'calls (for/list ([n nodes]) (render-entry n))))))
  (fprintf out "var profile_data = ")
  (write-json dict out))
