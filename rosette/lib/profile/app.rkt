#lang racket

(require (for-syntax racket/syntax (only-in racket for/list)))

(provide app (rename-out [app #%app] [app #%plain-app]))

(define-syntax (app stx)
  (syntax-case stx ()
    [(_ proc arg ...)
     (let* ([p (format-id #'proc "~a" (generate-temporary))]
            [src (syntax-source stx)]
            [pos (list (if (symbol? src) #`(quote #,src) src) (syntax-line stx) (syntax-column stx))]
            [args (syntax->list #'(arg ...))]
            [xs (for/list ([e args])
                  (if (keyword? (syntax-e e))
                      e
                      (format-id e "~a" (generate-temporary))))])
       (quasisyntax/loc stx
         (let*-values ([(#,p) proc]
                       #,@(for/list ([x xs][a args] #:when (identifier? x)) #`((#,x) #,a))
                       [(out cpu real gc) (time-apply (thunk (#%app #,p #,@xs)) null)])
           (record!
            (list #,@pos)
            #,p (list #,@(for/list ([x xs] #:when (identifier? x)) x)) out
            cpu real gc)
           (apply values out))))]))


(define (record! loc p in out cpu real gc)
  (printf "~a: ~a ~a -> ~a, ~a (cpu), ~a (real), ~a (gc)\n"
          loc p in out cpu real gc))

