#lang racket

(require (for-syntax racket/syntax (only-in racket for/list)))

(provide app (rename-out [app #%app] [app #%plain-app]) profile-thunk)

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


;; profile entries
(struct profile-node (location procedure inputs outputs cpu real gc children) #:transparent #:mutable)

;; display a profile entry
(define (display-profile node [level 0])
  (match-define (profile-node _ proc _ _ _ real _ children) node)
  (define indent (string-join (for/list ([i level]) "  ") ""))
  (printf "~a* ~a (~v msec)\n" indent proc real)  ;├─
  (for ([c children]) (display-profile c (add1 level))))

;; profile stack
(struct profile-stack ([frames #:mutable] root) #:transparent)
(define (new-state)
  (let ([root (profile-node 'root #f #f #f #f #f #f '())])
    (profile-stack (list root) root)))
(define profile-state (make-parameter (new-state)))

;; record a procedure entry
;; (called after all arguments to the procedure have been evaluated, but
;; before the procedure is invoked)
(define (record-enter! loc proc in)
  (let ([entry (profile-node loc proc in #f #f #f #f '())]
        [parent (car (profile-stack-frames (profile-state)))])
    (set-profile-node-children! parent (cons entry (profile-node-children parent)))
    (set-profile-stack-frames! (profile-state) (cons entry (profile-stack-frames (profile-state))))))
;; record a method exit
;; (called after a procedure returns)
(define (record-exit! out cpu real gc)
  (let ([entry (car (profile-stack-frames (profile-state)))])
    (set-profile-node-outputs! entry out)
    (set-profile-node-cpu! entry cpu)
    (set-profile-node-real! entry real)
    (set-profile-node-gc! entry gc)
    (set-profile-stack-frames! (profile-state) (cdr (profile-stack-frames (profile-state))))))


; Executes the given thunk and prints the profile data generated during
; execution.
(define (profile-thunk thunk)
  (define state (new-state))
  (define ret (parameterize ([profile-state state])
                (thunk)))
  (for ([c (profile-node-children (profile-stack-root state))])
    (display-profile c))
  ret)
