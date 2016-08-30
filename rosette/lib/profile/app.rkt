#lang racket

(require "record.rkt"
         (for-syntax racket/syntax (only-in racket for/list)))

(provide (rename-out [app #%app] [app #%plain-app]))

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
         (let ([#,p proc]
               #,@(for/list ([x xs][a args] #:when (identifier? x)) #`(#,x #,a)))
           (record-enter! (list #,@pos) #,p (list #,@(for/list ([x xs] #:when (identifier? x)) x)))
           (let-values ([(out cpu real gc) (time-apply (thunk (#%app #,p #,@xs)) null)])
             (record-exit! out cpu real gc)
             (apply values out)))))]))


