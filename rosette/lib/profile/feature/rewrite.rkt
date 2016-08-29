#lang racket

(require racket/syntax syntax/parse "instrument.rkt")

(provide make-symbolic-profile-compile-handler)


(define-namespace-anchor orig-namespace)

(define (make-symbolic-profile-compile-handler)
  (define orig (current-compile))
  (define reg (namespace-module-registry (current-namespace)))
  (define phase (namespace-base-phase (current-namespace)))
  (namespace-attach-module (namespace-anchor->namespace orig-namespace)
                           'racket/base)
  (namespace-attach-module (namespace-anchor->namespace orig-namespace)
                           'rosette/profile/rewrite)
  (namespace-attach-module (namespace-anchor->namespace orig-namespace)
                           'rosette/profile/instrument)

  (lambda (e immediate-eval?)
    (orig (let ([e2 (expand-syntax
                     (if (syntax? e)
                         e
                         (namespace-syntax-introduce
                          (datum->syntax #f e))))])
            (insert-instrumentation e2))
          immediate-eval?)))

(define orig-inspector (variable-reference->module-declaration-inspector
                        (#%variable-reference)))
(define (rearm orig new)
  (syntax-rearm new orig))
(define (disarm orig)
  (syntax-disarm orig orig-inspector))
(define (add-props props-stx stx)
  (define prop-keys (syntax-property-symbol-keys props-stx))
  (rearm props-stx
         (for/fold ([stx stx])
             ([k prop-keys])
           (syntax-property stx k (syntax-property props-stx k)))))

; must return syntax that *replaces* the invocation
#|
(define (instrument invocation)
  (syntax-parse invocation
    [((~and app #%plain-app) x:id y ...)
     (with-syntax* ([(y* ...) (generate-temporaries #'(y ...))])
       #'(let-values ([(y*) y] ...)
           (begin
             (#%plain-app printf "invoking ~v(~v)\n" x)
             (app x y* ...))))]))
|#
(define (instrument invocation)
  (syntax-parse invocation
    [((~and app #%plain-app) x:id y ...)
     #'(begin
         (app printf "invoking ~v\n" x)
         (app x y ...))]))

(define indent-level -1)
(define trace-instrumentation? #f)
(define (printfi . args)
  (when trace-instrumentation?
    (printf (string-join (for/list ([i indent-level]) "  ") ""))
    (apply printf args)))
(define (insert-instrumentation stx)
  (set! indent-level (add1 indent-level))
  (printfi "insert-instrumentation ~v\n" stx)
  (define-syntax-rule (stx/rearm new)
    (add-props stx (syntax/loc stx new)))
  (begin0
  (syntax-parse (disarm stx)
    #:literal-sets (kernel-literals)
    ; Function applications
    [((~and app #%plain-app) x:id y ...)
     (printfi "app ~v\n" (syntax->datum #'x))
     (with-syntax* ([x* (insert-instrumentation #'x)]
                    [(y* ...) (map insert-instrumentation (syntax->list #'(y ...)))]
                    [inst (instrument #'(app x* y* ...))])
       (printfi "orig: ~v\n" (syntax-e #'(app x* y* ...)))
       (printfi "inst: ~v\n" (syntax-e #'inst))
       #'inst)]  ; TODO: why does rearming inst not work?
    ; Things not to traverse into
    [((~or #%require #%provide quote quote-syntax #%top #%variable-reference
           #%declare begin-for-syntax define-syntaxes)
      . x)
     (printfi "never\n")
     stx]
    ; Things to traverse entirely
    [((~and head (~or #%expression begin begin0 #%plain-app if
                      with-continuation-mark))
      x ...)
     (printfi "always\n")
     (with-syntax ([(x* ...) (map insert-instrumentation (syntax->list #'(x ...)))])
       (stx/rearm (head x* ...)))]
    ; Traverse partially
    [((~and head set!) var val)
     (printfi "maybe: set!\n")
     (with-syntax ([val* (insert-instrumentation #'val)])
       (stx/rearm (head var val*)))]
    [((~and head (~or #%plain-lambda define-values))
      formals body ...)
     (printfi "maybe: lambda/define\n")
     (with-syntax ([(body* ...) (map insert-instrumentation (syntax->list #'(body ...)))])
       (stx/rearm (head formals body* ...)))]
    [((~and head case-lambda) [formals body ...] ...)
     (printfi "maybe: case-lambda\n")
     (with-syntax ([((body* ...) ...)
                    (for/list ([clause-body (syntax->list #'((body ...) ...))])
                      (map insert-instrumentation (syntax->list clause-body)))])
       (stx/rearm (head [formals body* ...] ...)))]
    [((~and head (~or let-values letrec-values)) ([lhs rhs] ...) body ...)
     (printfi "maybe: let/letrec\n")
     (with-syntax ([(rhs*  ...) (map insert-instrumentation (syntax->list #'(rhs  ...)))]
                   [(body* ...) (map insert-instrumentation (syntax->list #'(body ...)))])
       (stx/rearm (head ([lhs rhs*] ...) body* ...)))]
    [((~and head letrec-syntaxes+values) stx-bindings ([lhs rhs] ...) body ...)
     (printfi "maybe: letrec-syntaxes\n")
     (with-syntax ([(rhs*  ...) (map insert-instrumentation (syntax->list #'(rhs  ...)))]
                   [(body* ...) (map insert-instrumentation (syntax->list #'(body ...)))])
       (stx/rearm (head stx-bindings ([lhs rhs*] ...) body* ...)))]
    [((~and head (~or module
                      module*
                      (~literal module #:phase (namespace-base-phase))
                      (~literal module* #:phase (namespace-base-phase)))) name lang m-b-form)
     (printfi "module ~v\n" (syntax-e #'lang))
     (syntax-parse (disarm #'m-b-form)
       [(m-b body ...)
        (with-syntax ([(body* ...) (map insert-instrumentation (syntax->list #'(body ...)))])
          (stx/rearm (head name lang (m-b body* ...))))])]
    [x:id
     (printfi "maybe: id\n")
     (stx/rearm x)]
    [_
     (printfi "no match\n")
     stx])
  (set! indent-level (sub1 indent-level))))
