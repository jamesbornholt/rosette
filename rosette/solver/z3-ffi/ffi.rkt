#lang racket

(require ffi/unsafe ffi/unsafe/define racket/runtime-path
         racket/syntax (only-in racket [< racket/<] [- racket/-]))

;(provide (except-out (all-defined-out) define-ops printf-smt))
(provide (all-defined-out))


(define-runtime-path libz3-path (build-path ".." ".." ".." "bin" "libz3.dylib"))

(define libz3 (ffi-lib (path-replace-suffix libz3-path "")))

(define-cpointer-type _z3_config)
(define-cpointer-type _z3_context)
(define-cpointer-type _z3_sort)
(define-cpointer-type _z3_symbol)
(define-cpointer-type _z3_ast)
(define-cpointer-type _z3_solver)
(define-cpointer-type _z3_model)
(define-cpointer-type _z3_func_decl)
(define _z3_string _string/utf-8)
(define _z3_lbool (_enum '(FALSE = 4294967295 UNDEF = 0 TRUE)))

(define-ffi-definer define-z3 libz3)

(define-z3 Z3_mk_config (_fun -> _z3_config))
(define-z3 Z3_mk_context (_fun _z3_config -> _z3_context))

(define-z3 Z3_mk_string_symbol (_fun _z3_context _z3_string -> _z3_symbol))

(define-z3 Z3_mk_bool_sort (_fun _z3_context -> _z3_sort))

(define-z3 Z3_mk_true (_fun _z3_context -> _z3_ast))
(define-z3 Z3_mk_false (_fun _z3_context -> _z3_ast))

(define-z3 Z3_mk_const (_fun _z3_context _z3_symbol _z3_sort -> _z3_ast))
(define-z3 Z3_mk_func_decl (_fun _z3_context _z3_symbol _uint (_list i _z3_sort) _z3_sort -> _z3_func_decl))
(define-z3 Z3_mk_app (_fun _z3_context _z3_func_decl _uint (_list i _z3_ast) -> _z3_ast))

(define-z3 Z3_mk_not (_fun _z3_context _z3_ast -> _z3_ast))
(define-z3 Z3_mk_and (_fun _z3_context _uint (_list i _z3_ast) -> _z3_ast))
(define-z3 Z3_mk_or (_fun _z3_context _uint (_list i _z3_ast) -> _z3_ast))
(define-z3 Z3_mk_implies (_fun _z3_context _z3_ast _z3_ast -> _z3_ast))
(define-z3 Z3_mk_iff (_fun _z3_context _z3_ast _z3_ast -> _z3_ast))
(define-z3 Z3_mk_ite (_fun _z3_context _z3_ast _z3_ast _z3_ast -> _z3_ast))
(define-z3 Z3_mk_eq (_fun _z3_context _z3_ast _z3_ast -> _z3_ast))

(define-z3 Z3_mk_solver (_fun _z3_context -> _z3_solver))
(define-z3 Z3_solver_assert (_fun _z3_context _z3_solver _z3_ast -> _void))
(define-z3 Z3_solver_check (_fun _z3_context _z3_solver -> _z3_lbool))
(define-z3 Z3_solver_get_model (_fun _z3_context _z3_solver -> _z3_model/null))
(define-z3 Z3_solver_reset (_fun _z3_context _z3_solver -> _void))
(define-z3 Z3_solver_inc_ref (_fun _z3_context _z3_solver -> _void))
(define-z3 Z3_solver_push (_fun _z3_context _z3_solver -> _void))
(define-z3 Z3_solver_pop (_fun _z3_context _z3_solver _uint -> _void))


(define-z3 Z3_model_get_num_consts (_fun _z3_context _z3_model -> _uint))
(define-z3 Z3_model_get_const_decl (_fun _z3_context _z3_model _uint -> _z3_func_decl))
(define-z3 Z3_model_get_const_interp (_fun _z3_context _z3_model _z3_func_decl -> _z3_ast/null))
(define-z3 Z3_get_bool_value (_fun _z3_context _z3_ast -> _z3_lbool))



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

(define (declare-fun id domain range)
  (printf-smt "(declare-fun ~a ~a ~a)" id domain range))
                     
(define (define-const id type body)
  (printf-smt "(define-fun ~a () ~a ~a)" id type body))

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