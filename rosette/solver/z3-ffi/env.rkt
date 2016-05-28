#lang racket

(require racket/syntax 
         "ffi.rkt" (prefix-in $ "ast.rkt")
         "../../base/core/term.rkt" 
         (only-in "../../base/core/bool.rkt" @boolean?)
         (only-in "../../base/core/bitvector.rkt" bitvector? bitvector-size)
         (only-in "../../base/core/real.rkt" @integer? @real?))

(provide (rename-out [make-env env]) (except-out (struct-out env) env) ref! clear!)

(struct env (ctx decls ids defs sorts) #:transparent)

(define (make-env ctx)
  (env ctx (make-hash) (make-hash) (make-hash) (make-hash)))

(define (z3-id base n) (format "~a~a" base n))

(define (make-z3-sort t env)
  (match t
    [(== @boolean?) (Z3_mk_bool_sort (env-ctx env))]
    [_ (error 'make-z3-sort "unsupported type ~a" t)]))

(define (z3-sort t env)
  (hash-ref! (env-sorts env) t (thunk (make-z3-sort t env))))

; Clears the given environment of bindings for all Rosette
; values bound to an SMT identifier whose integer suffix is
; greater or equal to the given value.  Note that every
; Rosette value in the given dictionary has a unique integer
; suffix if they are created via the ref! macro.
(define (clear! env n)
  (define to-evict
    (for/list ([(k v) (in-hash (env-ids env))]
               #:when (>= (string->number (substring v 1)) n))
      k))
  (for ([k to-evict])
    (hash-remove! (env-decls env) k)
    (hash-remove! (env-ids env) k)
    (when (hash-has-key? (env-defs env) k)
      (hash-remove! (env-defs env) k))))

; in the enc case, enc should evaluate to a z3_ast, which
; will be stored in the defs.
(define-syntax ref!
  (syntax-rules ()
    [(_ env val) 
     (let ([decls (env-decls env)]
           [v val])
       (or (dict-ref decls v #f)         
           (let* ([id (z3-id 'c (dict-count decls))]
                  [t (term-type v)]
                  [domain (map (curryr z3-sort env) (solvable-domain t))]
                  [range (z3-sort (solvable-range t) env)]
                  [ret ($declare-fun (env-ctx env) id domain range)])
             (dict-set! (env-ids env) v id)
             (dict-set! decls v ret)
             ret)))]
    [(_ env val enc)
     (let ([defs (env-defs env)]
           [v val])
       (or (dict-ref defs v #f)
           (let ([e enc])
             (dict-set! defs v e)
             e)))]))
