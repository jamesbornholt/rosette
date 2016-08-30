#lang racket/base

(provide symbolic-profile-compile-handler)

(define (make-symbolic-profile-compile-handler)
  (define orig (current-compile))
  (lambda (e immediate-eval?)
    (orig (let ([e2 (if (syntax? e)
                        e
                        (namespace-syntax-introduce
                         (datum->syntax #f e)))])
            (insert-instrumentation e2))
          immediate-eval?)))

(define symbolic-profile-compile-handler
  (make-symbolic-profile-compile-handler))

(define (id=? id name)
  (and (identifier? id) (equal? (syntax-e id) name)))

(define (insert-instrumentation stx)
  (syntax-case stx ()
    [(mod id lang (mod-begin forms ...))
     (and (id=? #'mod 'module) (id=? #'lang 'rosette) (id=? #'mod-begin '#%module-begin))
     (begin
       (printf "INSTRUMENTING ~a\n" #'id)
       (quasisyntax/loc stx
         (mod id lang
              #,(datum->syntax #f (syntax->datum #'(mod-begin (require rosette/lib/profile/app) forms ...))))))]
    [_ stx]))






