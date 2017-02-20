#lang rosette

(require racket/require (matching-identifiers-in #rx"^node/.+$" "../lang/ast.rkt")
         (only-in "../lang/ast.rkt" relation-name relation-arity)
         "../lang/bounds.rkt" "../lang/universe.rkt"
         "matrix.rkt" "matrix-ops.rkt" "interpretation.rkt"
         rosette/lib/match
         (prefix-in $ racket ))

(provide interpret interpret*)

(define (interpret formula bounds)
  (define interp (instantiate-bounds bounds))
  (interpret* formula interp))
(define (interpret* formula interp #:cache? [cache? #f])
  (match-define (interpretation universe entries) interp)
  (define relations (make-hash entries))
  (interpret-rec formula universe relations (if cache? (make-hash) #f)))


(define (interpret-rec formula universe relations cache)
  ($if cache
       (if (hash-has-key? cache formula)
           (hash-ref cache formula)
           (let ([ret (interpret-body formula universe relations cache)])
             (hash-set! cache formula ret)
             ret))
       (interpret-body formula universe relations cache)))

(define (interpret-body formula universe relations cache)
  (match formula
    [(node/expr/op arity args)
     (let ([args* (for/list ([a args]) (interpret-rec a universe relations cache))])
       (interpret-expr-op universe formula args*))]
    [(node/expr/relation arity name)
     (hash-ref relations formula (thunk (error 'interpret "unbound relation ~a" formula)))]
    [(node/expr/constant arity type)
     (interpret-constant universe type)]
    [(node/expr/comprehension arity decls f)
     (let ([decls* (for/list ([d decls]) (cons (car d) (interpret-rec (cdr d) universe relations cache)))])
       (interpret-comprehension universe relations decls* f cache))]
    [(node/formula/op args)
     (interpret-formula-op universe relations formula args cache)]
    [(node/formula/quantified quantifier decls f)
     (let ([decls* (for/list ([d decls]) (cons (car d) (interpret-rec (cdr d) universe relations cache)))])
       (interpret-quantifier universe relations quantifier decls* f cache))]
    [(node/formula/multiplicity mult expr)
     (let ([expr* (interpret-rec expr universe relations cache)])
       (interpret-multiplicity universe mult expr*))]))


(define (interpret-constant universe type)
  (match type
    ['none (matrix (make-list (universe-size universe) #f))]
    ['univ (matrix (make-list (universe-size universe) #t))]
    ['iden (let ([size (universe-size universe)])
             (matrix (for*/list ([i size][j size]) (= i j))))]))


(define (interpret-expr-op universe op args)
  (match op
    [(? node/expr/op/+?)
     (matrix/nary-op universe matrix/or args)]
    [(? node/expr/op/&?)
     (matrix/nary-op universe matrix/and args)]
    [(? node/expr/op/-?)
     (matrix/nary-op universe matrix/difference args)]
    [(? node/expr/op/->?)
     (matrix/nary-op universe matrix/cross args)]
    [(? node/expr/op/~?)
     (matrix/transpose universe (first args))]
    [(? node/expr/op/join?)
     (matrix/nary-op universe matrix/dot args)]
    [(? node/expr/op/^?)
     (matrix/closure universe (first args))]
    [(? node/expr/op/*?)
     (let ([iden (interpret-constant universe 'iden)]
           [^A   (matrix/closure universe (first args))])
       (matrix/or universe ^A iden))]
    [(? node/expr/op/<:?)
     (matrix/domain universe (first args) (second args))]
    [(? node/expr/op/:>?)
     (matrix/range universe (first args) (second args))]))


(define (interpret-comprehension universe relations decls f cache)
  (define usize (universe-size universe))
  (define (comprehension* decls pre)
    (if (null? decls)
        (list (and pre (interpret-rec f universe relations (and cache (hash-copy cache)))))
        (match-let ([(cons v r) (car decls)])
          (append* (for/list ([i usize][val (matrix-entries r)])
                     (if ($false? val)
                         (make-list (expt usize (sub1 (length decls))) #f)
                         (begin
                           (hash-set! relations v (singleton-matrix universe i))
                           (begin0
                             (comprehension* (cdr decls) (and pre val))
                             (hash-remove! relations v)))))))))
  (matrix (comprehension* decls #t)))


(define (interpret-formula-short-circuit rec args op iden !iden)
  (let loop ([args args][val iden])
    (if (null? args)
        val
        (let ([v (op val (rec (car args)))])
          (if ($equal? v !iden)
              !iden
              (loop (cdr args) v))))))
(define (interpret-formula-op universe relations op args cache)
  (define rec (curryr interpret-rec universe relations cache))
  (match op
    [(? node/formula/op/&&?)
     (interpret-formula-short-circuit rec args && #t #f)]
    [(? node/formula/op/||?)
     (interpret-formula-short-circuit rec args || #f #t)]
    [(? node/formula/op/=>?)
     (or (not (rec (first args))) (rec (second args)))]
    [_ (let ([args ($map rec args)])
         (match op
           [(? node/formula/op/in?)
            (let ([A (first args)] [B (second args)])
              (matrix/subset? universe A B))]
           [(? node/formula/op/=?)
            (let ([A (first args)] [B (second args)])
              (matrix/equal? universe A B))]
           [(? node/formula/op/!?)
            (not (first args))]))]))


; quantifier: 'all or 'some
; decls: (listof (cons ast/node/relation matrix?)) binds the domains of the quantified variables
; f: the predicate
(define (interpret-quantifier universe relations quantifier decls f cache)
  (define usize (universe-size universe))
  (define (evaluate-quantifier op iden conn)
    (define (rec decls)
      (if (null? decls)
          (interpret-rec f universe relations (and cache (hash-copy cache)))
          (match-let ([(cons v r) (car decls)])
            (apply op (for/list ([i usize][val (matrix-entries r)])
                          (if ($false? val)
                              iden
                              (begin
                                (hash-set! relations v (singleton-matrix universe i))
                                (begin0
                                  (conn val (rec (cdr decls)))
                                  (hash-remove! relations v)))))))))
    (rec decls))
  (case quantifier
    ['all  (evaluate-quantifier && #t =>)]
    ['some (evaluate-quantifier || #f &&)]))


(define (interpret-multiplicity universe mult expr)
  (match mult
    ['one  (matrix/one? universe expr)]
    ['no   (not (matrix/some? universe expr))]
    ['some (matrix/some? universe expr)]
    ['lone (or (not (matrix/some? universe expr)) (matrix/one? universe expr))]))