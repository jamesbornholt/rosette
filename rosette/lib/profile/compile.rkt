#lang racket

(require compiler/compile-file (only-in racket [#%require req]))
(provide symbolic-profile-compile-handler)

(define-namespace-anchor orig-namespace)

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
     (let ([body (map disarm (syntax->list #'(forms ...)))])
       ;(printf "INSTRUMENTING ~a\n" #'id)
       (quasisyntax/loc stx
         (mod id lang (mod-begin (req rosette/lib/profile/app) #,@body))))]
    [_ stx]))

(define orig-insp (variable-reference->module-declaration-inspector (#%variable-reference)))

(define (app-identifier? id)
  (or (equal? id '#%plain-app) (equal? id '#%app)))

(define (disarm stx)
  ;(printf "   INSTRUMENTING ~a\n" stx)
  (syntax-disarm stx orig-insp)
  (syntax-case (syntax-disarm stx orig-insp) ()
    [(e rest ...)
     (quasisyntax/loc stx (#,(disarm #'e) #,@(map disarm (syntax->list #'(rest ...)))))]
    [_ stx]))






