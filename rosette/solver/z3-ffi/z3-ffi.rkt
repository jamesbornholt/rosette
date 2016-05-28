#lang racket

(require racket/runtime-path 
         "env.rkt" "cmd.rkt"
         (only-in "ast.rkt" check-sat reset push pop)
         "../solver.rkt" "../solution.rkt" 
         (only-in racket [remove-duplicates unique])
         "ffi.rkt"
         ; (only-in "ffi.rkt" reset set-option check-sat get-model get-unsat-core push pop)
         (only-in "../../base/core/term.rkt" term term? term-type)
         (only-in "../../base/core/bool.rkt" @boolean?)
         (only-in "../../base/core/bitvector.rkt" bitvector? bv?)
         (only-in "../../base/core/real.rkt" @integer? @real?))

(provide (rename-out [make-z3-ffi z3-ffi]) z3-ffi?)

(define (make-z3-ffi)
  ;(unless (file-exists? z3-path)
  ;  (raise-user-error 'z3 "Failed to locate z3 binary at '~a'" z3-path))
  (let* ([cfg (Z3_mk_config)][ctx (Z3_mk_context cfg)][solver (Z3_mk_solver ctx)])
    (Z3_solver_inc_ref ctx solver)
    (z3-ffi ctx solver '() '() '() (env ctx) '())))
  
(struct z3-ffi (context solver asserts mins maxs env level)
  #:mutable
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "#<z3-ffi>"))]
  #:methods gen:solver
  [
   (define (solver-assert self bools)
     (set-z3-ffi-asserts! self 
      (append (z3-ffi-asserts self)
              (for/list ([b bools] #:unless (equal? b #t))
                (unless (or (boolean? b) (and (term? b) (equal? @boolean? (term-type b))))
                  (error 'assert "expected a boolean value, given ~s" b))
                b))))

   (define (solver-minimize self nums)
     (set-z3-ffi-mins! self (append (z3-ffi-mins self) (numeric-terms nums 'solver-minimize))))
   
   (define (solver-maximize self nums)
     (set-z3-ffi-maxs! self (append (z3-ffi-maxs self) (numeric-terms nums 'solver-maximize))))
   
   (define (solver-clear self)
     (solver-clear-stacks! self)
     (solver-clear-env! self)
     (reset (z3-ffi-context self) (z3-ffi-solver self)))
   #;(define (solver-clear self)
     (error 'solver-clear "NYI"))
   
   (define (solver-shutdown self)
     (solver-clear self))
   #;(define (solver-shutdown self)
     (error 'solver-shutdown "NYI"))

   (define (solver-push self)
     (match-define (z3-ffi ctx solver (app unique asserts) (app unique mins) (app unique maxs) env level) self)
     (encode env solver asserts mins maxs)
     (push ctx solver)
     (solver-clear-stacks! self)
     (set-z3-ffi-level! self (cons (dict-count (env-decls env)) level)))
   #;(define (solver-push self)
     (error 'solver-push "NYI"))
   
   (define (solver-pop self [k 1])
     (match-define (z3-ffi ctx solver _ _ _ env level) self)
     (when (or (<= k 0) (> k (length level)))
       (error 'solver-pop "expected 1 < k <= ~a, given ~a" (length level) k))
     (pop ctx solver k)
     (solver-clear-stacks! self)
     (for ([lvl level][i k])
       (clear! env lvl))
     (set-z3-ffi-level! self (drop level k)))
   #;(define (solver-pop self [k 1])
     (error 'solver-pop "NYI"))
     
   (define (solver-check self)
     (match-define (z3-ffi ctx solver (app unique asserts) (app unique mins) (app unique maxs) env _) self)
     (cond [(ormap false? asserts) (unsat)]
           [else (encode env solver asserts mins maxs)
                 (match (check-sat (env-ctx env) solver)
                   ['TRUE (decode env solver)]
                   [x (unsat)])]))
   #;(define (solver-check self)
     (error 'solver-check "NYI"))
   
   #;(define (solver-debug self)
     (match-define (z3 server (app unique asserts) _ _ _ _) self)
     (cond [(ormap false? asserts) (unsat (list #f))]
           [else (solver-clear-env! self)
                 (server-write (z3-server self) (reset-core-options))
                 (server-write
                  server
                  (begin (encode-for-proof (z3-env self) asserts)
                         (check-sat)
                         (get-unsat-core)))
                 (server-read server (decode (z3-env self)))]))
   (define (solver-debug self)
     (error 'solver-debug "NYI"))
   ])

(define (numeric-terms ts caller)
  (for/list ([t ts] #:unless (or (real? t) (bv? t)))
    (match t
      [(term _ (or (== @integer?) (== @real?) (? bitvector?))) t]
      [_ (error caller "expected a numeric term, given ~s" t)])))

(define (solver-clear-stacks! self)
  (set-z3-ffi-asserts! self '())
  (set-z3-ffi-mins! self '())
  (set-z3-ffi-maxs! self '()))

(define (solver-clear-env! self)
  (set-z3-ffi-env! self (env (z3-ffi-context self)))
  (set-z3-ffi-level! self '()))
  