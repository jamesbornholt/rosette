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
     (with-syntax ([body (strip-context #`(mod-begin (require rosette/lib/profile/instrument)
                                                     forms ...))])
       (printf "INSTRUMENTING ~a\n" #'id)
       (quasisyntax/loc stx
         (mod id lang body)))] 
    [_ stx]))

(define (strip-context stx)
  (syntax-case stx ()
    [(e0 e ...)
     (datum->syntax #f `(,(strip-context #'e0) ,@(map strip-context (syntax->list #'(e ...)))) stx)]
    [e
     (identifier? #'e)
     (datum->syntax #f (syntax-e #'e) #'e)]
    [_ stx]))




