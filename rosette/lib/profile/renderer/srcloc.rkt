#lang racket

(require racket/date)
(provide (all-defined-out))

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
