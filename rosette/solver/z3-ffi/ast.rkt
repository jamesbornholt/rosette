#lang racket

(require "ffi.rkt")

(provide (all-defined-out))

#|
; Reads the SMT solution from current-input-port.
; The solution consist of 'sat or 'unsat, followed by  
; followed by a suitably formatted s-expression.  The 
; output of this procedure is a hashtable from constant 
; identifiers to their values (if the solution is 'sat);
; a non-empty list of assertion identifiers that form an
; unsatisfiable core (if the solution is 'unsat and a 
; core was extracted); or #f (if the solution is 
; 'unsat and no core was extracted).
(define (read-solution)
  (define port (current-input-port))
  (match (read port)
    [(== 'sat)
     (let loop ()
       (match (read port)
         [(list (== 'objectives) _ ...) (loop)]
         [(list (== 'model) def ...)
          (for/hash ([d def])
            (match d
              [(list (== 'define-fun) c '() _ v) (values c v)]
              [(list (== 'define-fun) c _ ...) (values c d)]))]
         [other (error 'solution "expected model, given ~a" other)]))]
    [(== 'unsat) 
     (match (read port) 
       [(list (? symbol? name) ...) name]
       [_ #f])]
    [other (error 'solution "unrecognized solver output: ~a" other)]))

; Prints all smt commands to current-output-port.
(define-syntax-rule (printf-smt arg ...)
  (begin 
    ;(fprintf (current-error-port) arg ...)(fprintf (current-error-port) "\n")
    (printf arg ...)))
|#
#|
; Commands
(define (set-option opt val) (printf-smt "(set-option ~a ~a)" opt val))

(define (set-logic l)    (printf-smt "(set-logic ~a)" l))
(define (check-sat)      (printf-smt "(check-sat)\n"))
(define (get-model)      (printf-smt "(get-model)\n"))
(define (get-unsat-core) (printf-smt "(get-unsat-core)\n"))
(define (get-info kw)    (printf-smt "(get-info ~a)\n" kw))

(define (reset)          (printf-smt "(reset)\n"))
(define (push [n 1])     (printf-smt "(push ~a)\n" n))
(define (pop [n 1])      (printf-smt "(pop ~a)\n" n))

(define assert 
  (case-lambda [(e)     (printf-smt "(assert ~a)" e)]
               [(e id)  (printf-smt "(assert (! ~a :named ~a))" e id)]))

(define (minimize t)    (printf-smt "(minimize ~a)" t))
(define (maximize t)    (printf-smt "(maximize ~a)" t))

; Declarations and definitions
(define (declare-const id type)
  (printf-smt "(declare-const ~a ~a)" id type))
|#

(define (declare-fun ctx id domain range)
  (define sym (Z3_mk_string_symbol ctx id))
  (Z3_mk_func_decl ctx sym (length domain) domain range))

;fun is a func_decl
; args is a list of asts
(define (app ctx f . args)
  (Z3_mk_app ctx f (length args) args))

(define (true ctx)
  (Z3_mk_true ctx))
(define (false ctx)
  (Z3_mk_false ctx))
(define (not ctx x)
  (Z3_mk_not ctx x))
(define (and ctx . xs)
  (Z3_mk_and ctx (length xs) xs))
(define (or ctx . xs)
  (Z3_mk_or ctx (length xs) xs))
(define (=> ctx t1 t2)
  (Z3_mk_implies ctx t1 t2))
(define (<=> ctx t1 t2)
  (Z3_mk_iff ctx t1 t2))
(define (ite ctx t1 t2 t3)
  (Z3_mk_ite ctx t1 t2 t3))
(define (= ctx t1 t2)
  (Z3_mk_eq ctx t1 t2))

(define (assert ctx slvr a)
  (Z3_solver_assert ctx slvr a))
(define (check-sat ctx slvr)
  (Z3_solver_check ctx slvr))
(define (get-model ctx slvr)
  (Z3_solver_get_model ctx slvr))
(define (get-interp ctx model a)
  (Z3_model_get_const_interp ctx model a))
(define (get-bool-value ctx a)
  (Z3_get_bool_value ctx a))
(define (reset ctx slvr)
  (Z3_solver_reset ctx slvr))
(define (push ctx slvr)
  (Z3_solver_push ctx slvr))
(define (pop ctx slvr n)
  (Z3_solver_pop ctx slvr n))

; [@&& $and] [@|| $or] [@=> $=>] [@<=> $<=>] [ite $ite] [=? $=]

#|
; Applications of uninterpreted functions.
(define (app f . args)
  `(,f ,@args))

(define-syntax-rule (define-ops id ...)
  (define-values (id ...)
    (values (lambda e `(id ,@e)) ...)))

; Core theory
(define Bool 'Bool)
(define true 'true)
(define false 'false)
(define-ops not and or xor => ite =)
(define (<=> l r) (and (=> l r) (=> r l)))

; Bitvector theory
(define (BitVec size) `(_ BitVec ,size))
(define (bv val size)  (if (racket/< val 0)
                           (bvneg `(_ ,(format-symbol "bv~a" (racket/- val)) ,size))
                           `(_ ,(format-symbol "bv~a" val) ,size)))
(define-ops 
  bvnot bvand bvor bvxor 
  bvule bvult bvuge bvugt bvsle bvslt bvsge bvsgt
  bvneg bvadd bvsub bvmul bvsdiv bvudiv bvurem bvsrem bvsmod
  bvshl bvlshr bvashr concat) 

(define (extract i j s)
  `((_ extract ,i ,j) ,s))

(define (zero_extend i b)
  `((_ zero_extend ,i) ,b))

(define (sign_extend i b)
  `((_ sign_extend ,i) ,b))

; Int and Real theories
(define Int 'Int)
(define Real 'Real)
(define-ops
  + - * / div mod abs 
  < <= 
  is_int to_int to_real )
|#