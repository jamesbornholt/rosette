#lang racket

(require "record.rkt"
         (for-syntax racket/syntax (only-in racket for/list)))

(provide (rename-out [$app #%app] [$app #%plain-app] 
                     [$define define]
                     [$let let]
                     [$lambda lambda]
                     ))

(define-for-syntax (location stx)
  (let ([src (syntax-source stx)])
    (list (if (symbol? src) #`(quote #,src) src) (syntax-line stx) (syntax-column stx))))

(define-syntax ($app stx)
  (syntax-case stx ()
    [(_ proc arg ...)
     (let* ([p (format-id #'proc "~a" (generate-temporary))]
            [pos (location stx)]
            [args (syntax->list #'(arg ...))]
            [xs (for/list ([e args])
                  (if (keyword? (syntax-e e))
                      e
                      (format-id e "~a" (generate-temporary))))])
       (quasisyntax/loc stx
         (let ([#,p proc]
               #,@(for/list ([x xs][a args] #:when (identifier? x)) #`(#,x #,a)))
           (record-enter! (list #,@pos) #,p (list #,@(for/list ([x xs] #:when (identifier? x)) x)))
           ; `out` will always be a list of two values:
           ; * If the application does not produce an exception, the first value is #f
           ;   and the second is a list of the values returned from the application.
           ; * If the application does produce an exception, the first value is that exception
           ;   and the second is null.
           ; This arrangement enables collecting timing data even from applications that
           ; throw exceptions, which is common during symbolic evaluation.
           ; TODO: should we annotate the profle-node somehow to indicate an exception was thrown?
           (let-values ([(out cpu real gc)
                         (time-apply (thunk (with-handlers ([exn? (lambda (exn) (values exn null))])
                                              (call-with-values
                                               (thunk (#%app #,p #,@xs))
                                               (lambda e (values #f e))))) null)])
             (record-exit! (cadr out) cpu real gc)
             (if (false? (car out)) (apply values (cadr out)) (raise (car out)))))))]))

(define-syntax ($define stx)
  (syntax-case stx ()
    [(_ (id arg ...) body ...)
     (quasisyntax/loc stx
       (define id
         (record-source!
          (let () (define (id arg ...) body ...) id)
          (list #,@(location stx)))))]
    [(_ e ...) (quasisyntax/loc stx (define e ...))]))


(define-syntax ($lambda stx)
  (syntax-case stx ()
    [(_ e ...)
     (quasisyntax/loc stx
       (record-source!
        #,(quasisyntax/loc stx (lambda e ...))
        (list #,@(location stx))))]))

(define-syntax ($let stx)
  (syntax-case stx ()
    [(_ id e ...)
     (identifier? #'id)
     (quasisyntax/loc stx
       ((record-source!
         (letrec ([id (lambda e ...)]) id)
         (list #,@(location stx)))))]
    [(_ e ...) (quasisyntax/loc stx (let e ...))]))


     