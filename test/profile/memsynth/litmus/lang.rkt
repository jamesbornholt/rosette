#lang s-exp rosette

(provide (all-defined-out))

;; programs --------------------------------------------------------------------

; a program (actually an execution) is a list of actions.
; a thread is a list of actions.
(struct Program (threads) #:transparent)
(struct Thread (tid actions) #:transparent)  ; actions, thread ID
(struct Action (gid lid thd deps addr val) #:transparent)  ; global ID, thread-local ID, thread ID, local deps
(struct Read Action () #:transparent)
(struct Write Action () #:transparent)
(struct Fence Action (type) #:transparent)

; get all actions in a program
(define (all-actions P)
  (for*/list ([thd (Program-threads P)][act (Thread-actions thd)]) act))


;; tests -----------------------------------------------------------------------

; A litmus test consists of a test name, a program? struct, a postcondition
; and list of models that allow the test.
(struct litmus-test (name program post allowed) #:transparent)

(define-syntax define-litmus-test
  (syntax-rules ()
    [(_ name (thd ...))
     (define-litmus-test name (thd ...) #:post () #:allowed)]
    [(_ name (thd ...) #:allowed a ...)
     (define-litmus-test name (thd ...) #:post () #:allowed a ...)]
    [(_ name (thd ...) #:post p)
     (define-litmus-test name (thd ...) #:post p #:allowed)]
    [(_ name (thd ...) #:post p #:allowed a ...)
     (define name (litmus-test 'name (read-test (list 'thd ...)) 'p (list 'a ...)))]))

(define (litmus-test-allowed? model test)
  (or (equal? model 'any)
      (not (false? (member model (litmus-test-allowed test))))))

; parse a test into a program struct
(define (read-test t)
  (define gid 0)
  (Program
   (for/list ([thd t][tid (length t)])
     (Thread tid
             (for/list ([a thd][lid (length thd)])
               (define deps (match a
                              [(list _ _ _ d) d]
                              [_ '()]))
               (begin0
                 (match a
                   [(list 'R addr val _ ...) (Read  gid lid tid deps addr val)]
                   [(list 'W addr val _ ...) (Write gid lid tid deps addr val)]
                   [(list 'F type)           (Fence gid lid tid '()  0    0   type)]
                   [(list 'F)                (Fence gid lid tid '()  0    0   'sync)])
                 (set! gid (add1 gid))))))))
