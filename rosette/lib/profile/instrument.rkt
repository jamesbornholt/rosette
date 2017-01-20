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
     (quasisyntax/loc stx (record-apply! (list #,@(location stx)) proc arg ...))]))

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


     