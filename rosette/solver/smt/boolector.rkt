#lang racket

(require racket/runtime-path 
         "server.rkt" "cmd.rkt" "env.rkt" 
         "../solver.rkt" "../solution.rkt" 
         (only-in racket [remove-duplicates unique])
         (only-in "smtlib2.rkt" reset set-option check-sat get-model get-unsat-core push pop)
         (only-in "../../base/core/term.rkt" term term? term-type constant? expression constant)
         (only-in "../../base/core/bool.rkt" @boolean? @forall @exists)
         (only-in "../../base/core/bitvector.rkt" bitvector? bv? bv-value @extract @sign-extend @zero-extend)
         (only-in "../../base/core/real.rkt" @integer? @real?)
         (only-in "../../base/core/function.rkt" function-domain function-range function?)
         (only-in "../../base/core/type.rkt" type-of))

(provide (rename-out [make-boolector boolector]) boolector? boolector-available?)

(define-runtime-path boolector-path (build-path ".." ".." ".." "bin" "boolector"))
(define boolector-opts '("-m" "--smt2-model" "-i"))

(define (find-boolector)
  (if (file-exists? boolector-path)
      boolector-path
      (let ([boolector.exe-path (path-replace-suffix boolector-path ".exe")])
        (if (file-exists? boolector.exe-path)
          boolector.exe-path
          #f))))

(define (boolector-available?)
  (not (false? (find-boolector))))

(define (make-boolector)
  (define real-boolector-path (find-boolector))
  (if (and (false? real-boolector-path) (not (getenv "PLT_PKG_BUILD_SERVICE")))
      (error 'boolector "boolector binary is not available (expected to be at ~a)" (path->string (simplify-path boolector-path)))
      (boolector (server real-boolector-path boolector-opts set-default-options) '() '() '() (env) '())))
  
(struct boolector (server asserts mins maxs env level)
  #:mutable
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "#<boolector>"))]
  #:methods gen:solver
  [
   (define (solver-constructor self)
     make-boolector)
   
   (define (solver-features self)
     '(qf_bv qf_uf))
   
   (define (solver-assert self bools)
     (set-boolector-asserts! self 
      (append (boolector-asserts self)
              (for/list ([b bools] #:unless (equal? b #t))
                (unless (or (boolean? b) (and (term? b) (equal? @boolean? (term-type b))))
                  (error 'assert "expected a boolean value, given ~s" b))
                (boolector-typecheck b)
                b))))

   (define (solver-minimize self nums)
     (unless (null? nums)
       (error 'solver-minimize "boolector does not support optimization")))
   
   (define (solver-maximize self nums)
     (unless (null? nums)
       (error 'solver-maximize "boolector does not support optimization")))
   
   (define (solver-clear self)
     (solver-shutdown self))
   
   (define (solver-shutdown self)
     (solver-clear-stacks! self)
     (solver-clear-env! self)
     (server-shutdown (boolector-server self)))

   (define (solver-push self)
     (match-define (boolector server (app unique asserts) (app unique mins) (app unique maxs) env level) self)
     (server-write
      server
      (begin
        (encode env asserts mins maxs)
        (push)))
     (solver-clear-stacks! self)
     (set-boolector-level! self (cons (dict-count env) level)))
   
   (define (solver-pop self [k 1])
     (match-define (boolector server _ _ _ env level) self)
     (when (or (<= k 0) (> k (length level)))
       (error 'solver-pop "expected 1 < k <= ~a, given ~a" (length level) k))
     (server-write server (pop k))
     (solver-clear-stacks! self)
     (for ([lvl level][i k])
       (clear! env lvl))
     (set-boolector-level! self (drop level k)))
     
   (define (solver-check self)
     (match-define (boolector server (app unique asserts) (app unique mins) (app unique maxs) env _) self)
     (cond [(ormap false? asserts) (unsat)]
           [else (server-write
                  server
                  (begin (encode env asserts mins maxs)
                         (check-sat)))
                 (solver-clear-stacks! self)
                 (read-solution server env)]))
   
   (define (solver-debug self)
     (error 'solver-debug "boolector debug not supported"))])

(define (set-default-options server)
  void)

(define (numeric-terms ts caller)
  (for/list ([t ts] #:unless (or (real? t) (bv? t)))
    (match t
      [(term _ (or (== @integer?) (== @real?) (? bitvector?))) t]
      [_ (error caller "expected a numeric term, given ~s" t)])))

(define (solver-clear-stacks! self)
  (set-boolector-asserts! self '())
  (set-boolector-mins! self '())
  (set-boolector-maxs! self '()))

(define (solver-clear-env! self)
  (set-boolector-env! self (env))
  (set-boolector-level! self '()))

(define (boolector-typecheck v)
  (define (valid-type? t)
    (or (equal? t @boolean?)
        (bitvector? t)
        (and (function? t)
             (for/and ([d (in-list (function-domain t))]) (valid-type? d))
             (valid-type? (function-range t)))))
  (cond
    [(term? v)
     (unless (valid-type? (type-of v))
       (error 'boolector "boolector does not support values of type ~v (value: ~v)" (type-of v) v))
     (match v
       [(expression (or (== @forall) (== @exists)) vars body)
        (error 'boolector "boolector does not support quantified formulas (value: ~v)" v)]
       [(expression (== @extract) i j e)
        (boolector-typecheck e)]
       [(expression (or (== @sign-extend) (== @zero-extend)) v t)
        (boolector-typecheck v)]
       [(expression op es ...)
        (map boolector-typecheck es)]
       [_ #t])]
    [else
     (unless (or (boolean? v) (bv? v))
       (error 'boolector "boolector does not support value (expected boolean? or bv?): ~v" v))]))


; Reads the SMT solution from the server.
; The solution consists of 'sat or 'unsat, followed by  
; followed by a suitably formatted s-expression.  The 
; output of this procedure is a hashtable from constant 
; identifiers to their SMTLib values (if the solution is 'sat);
; a non-empty list of assertion identifiers that form an
; unsatisfiable core (if the solution is 'unsat and a 
; core was extracted); #f (if the solution is 
; 'unsat and no core was extracted); or 'unknown otherwise.
(define (read-solution server env #:unsat-core? [unsat-core? #f])
  (define m
    (decode
     (parameterize ([current-readtable (make-readtable #f #\# #\a #f)]) ; read BV literals as symbols
       (match (server-read server (read))
         [(== 'sat)
          (server-write server (get-model))
          (let loop ()
            (match (server-read server (read))
              [(list (== 'objectives) _ ...) (loop)]
              [(list (== 'model) def ...)
               (for/hash ([d def] #:when (and (pair? d) (equal? (car d) 'define-fun)))
                 (values (cadr d) d))]
              [other (error 'read-solution "expected model, given ~a" other)]))]
         [(== 'unsat)
          (if unsat-core?
              (begin
                (server-write server (get-unsat-core))
                (match (server-read server (read))
                  [(list (? symbol? name) ...) name]
                  [other (error 'read-solution "expected unsat core, given ~a" other)]))
              'unsat)]
         [(== 'unknown) 'unknown]
         [other (error 'read-solution "unrecognized solver output: ~a" other)]))
     env))
  ; fix up booleans being mapped to 1-bit bitvectors
  (match m
    [(model dict)
     (sat (for/hash ([(var val) (in-dict dict)])
            (values var (if (equal? (term-type var) @boolean?) (not (= (bv-value val) 0)) val))))]
    [_ m]))
