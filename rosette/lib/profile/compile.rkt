#lang racket/base

(require (only-in racket [require req]))
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

(define (insert-instrumentation stx)
  (syntax-case stx ()
    [(mod id lang (mod-begin forms ...))
     (and (identifier? #'mod) (identifier? #'lang) (identifier? #'mod-begin)
          (equal? (syntax->datum #'mod) 'module)
          (equal? (syntax->datum #'lang) 'rosette)
          (equal? (syntax->datum #'mod-begin) '#%module-begin))
     (begin
       (printf "INSTRUMENTING ~a\n" #'id)
       (quasisyntax/loc stx
         (mod id lang
              #,(datum->syntax #f
                               (syntax->datum #'(mod-begin (require rosette/lib/profile/app) forms ...))))))]
    [_ stx]))






