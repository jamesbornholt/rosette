#lang racket/base

(require syntax/strip-context)
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
     ; Create a fresh scope to ensure that the inserted `require` is bound to
     ; this file's `require` rather than one from the instrumented module
     (let ([introduce (make-syntax-introducer #t)])  ; create a fresh scope
       (with-syntax ([body #`(mod-begin
                              (#,(introduce #'require)  ; add fresh scope to cancel below
                               #,(replace-context #'lang #'rosette/lib/profile/instrument))
                              forms ...)])
         (printf "INSTRUMENTING ~a\n" #'id)
         (introduce  ; add fresh scope to the module
          (quasisyntax/loc stx
            (mod id lang body)))))]
    [_ stx]))
