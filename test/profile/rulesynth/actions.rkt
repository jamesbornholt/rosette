#lang rosette

(require rosette/lib/match rosette/lib/angelic "../bench.rkt")
(provide (all-defined-out) (rename-out [list-set @list-set]))

; ------------- List manipulation routines ------------- 
(define (list-set lst pos val)
  (bench
   (let-values ([(front back) (split-at lst pos)])
     (append front (cons val (cdr back))))
   (for/all ([lst lst]) 
    (map (lambda (i v) (if (= pos i) val v))
         (build-list (length lst) identity)
         lst))))

(define (remove-at lst pos)
  (bench
   (let-values ([(front back) (split-at lst pos)])
     (append front (rest back)))
   (for/all ([lst lst])
    (let loop ([i 0] [front empty] [back lst])
      (if (= i pos)
          (append front (cdr back))
          (loop (add1 i) (append front (list (car back))) (cdr back)))))))


; ------------- Tree manipulation routines ------------- 
(define (tree-ref term idx)
  (match idx
   [(cons head tail)
    (tree-ref (list-ref term head) tail)]
   ['() term]))

(define (tree-remove term idx)
  (match idx
   [(cons head '())
    (remove-at term head)]
   [(cons head tail)
    (list-set term head (tree-remove (list-ref term head) tail))]
   ['() (error "invalid index!")]))

(define (tree-replace term idx subterm)
  (match idx
    [(cons head tail)
     (list-set term head (tree-replace (list-ref term head) tail subterm))]
    ['() subterm]))

; ------------- Action programs ------------- 

; Action programs are lists of actions, which can refer  to expressions.  
; Each action in a program operates on the term produced by a previous action.  
; Each expression may refer only to subterms in the input term.  See below
; for examples of action programs.

(struct Action () #:transparent)
(struct Remove Action (idx) #:transparent)
(struct Replace Action (idx expr) #:transparent)

(struct Expr () #:transparent)
(struct Ref Expr (idx) #:transparent)
(struct Term Expr (op args) #:transparent)
(struct App Expr (op args) #:transparent)

; Interpreter for action programs, given as lists of actions. 
(define (interpret prog t0)
  (define (interpret-expr expr)
    (match expr
      [(Ref idx)      
       (tree-ref t0 idx)]
      [(Term op args) 
       (cons op (map interpret-expr args))]
      [(App op args)  
       (apply (match op ['+ +] ['* *] ['/ /] ['- -] ['= =])
              (map interpret-expr args))]))
  (define (interpret-acts acts t)  
    (match acts
      [(list) t]
      [(list inst rest ...) 
       (match inst
         [(Remove idx)       
          (interpret-acts rest (tree-remove t idx))]
         [(Replace idx expr) 
          (interpret-acts rest (tree-replace t idx (interpret-expr expr)))])]))
  (interpret-acts prog t0))

; ------------- Sketch generators ------------- 

; Constructs a symbolic expression of up to the given depth, 
; using the specified operators and terminals:
; > (expr-sketch '((1) (1 1 2 1)) '(+ *) 2)
(define (expr-sketch idxs ops depth)
  (if (<= depth 0) 
      (Ref (apply choose* idxs))
      (let ([left  (expr-sketch idxs ops (sub1 depth))]
            [right (expr-sketch idxs ops (sub1 depth))])
        (choose* left ; choose between the lower and current depth 
                 ((choose* App Term) (apply choose* ops) (list left right))))))   

; Creates a sketch for an action program with the given 
; number of actions, where all expressions have up to the 
; specified depth, and are constructed from the given 
; indices and operators.
(define (sketch idxs ops depth len)
  (for/list ([i len])
    (let ([idx (apply choose* idxs)])
      (choose* (Remove idx)
               (Replace idx (expr-sketch idxs ops depth))))))
 
; Returns a solution for the given sketch (if any) that 
; satisfies the given examples.
(define (synth sketch examples)
  (solve
   (for ([ex examples])
    (assert (equal? (interpret sketch (car ex)) (cdr ex))))))


  
; ------------- Examples ------------- ;

; Easy example: canceling constants (2 + -2 + x --> x)
(define co-ex (list
  (cons '(+ 1 -1 x) '(+ x))
  (cons '(+ -2 2 3 y) '(+ 3 y))
  ))

(define (apply-co t0)
  (define t1 (tree-remove t0 '(2)))
  (define t2 (tree-remove t1 '(1)))
  t2)

(define co-prog (list (Remove '(2)) (Remove '(1))))

; Medium example: combining like terms (5x + 3x --> 8x)
(define clt-ex (list
  (cons '(+ (* 3 x) (* 5 x)) '(+ (* 8 x)))
  (cons '(+ (* 2 y) (* 3 y) 4) '(+ (* 5 y) 4))
  (cons '(+ (* 1 3) (* -2 3) x) '(+ (* -1 3) x))
  ))

(define (apply-clt t0)
  (define t1 (tree-remove t0 '(2)))
  (define t2 (tree-replace t1 '(1 1) (+ (tree-ref t0 '(1 1)) (tree-ref t0 '(2 1)))))
  t2)

(define clt-prog
  (list (Remove '(2))
        (Replace '(1 1) (App '+ (list (Ref '(1 1)) (Ref '(2 1)))))))

; Medium/hard example: moving a denominator to the other side of the equation (3x = 4 --> x = 4/3)
(define mf-ex (list
  (cons '(= (+ (/ (* x) (* 3))) (+ 5)) '(= (+ (* x)) (* (+ 5) 3)))
  (cons '(= (+ (/ (* x y) (* 2))) (+ 3)) '(= (+ (* x y)) (* (+ 3) 2)))
  (cons '(= (+ (/ (* x) (* 4))) (+ 5 y)) '(= (+ (* x)) (* (+ 5 y) 4)))
  ))

(define (apply-mf t0)
  (define t1 (tree-replace t0 '(1 1) (tree-ref t0 '(1 1 1))))
  (define t2 (tree-replace t1 '(2) `(* ,(tree-ref t0 '(2)) ,(tree-ref t0 '(1 1 2 1)))))
  t2)

(define mf-prog 
  (list (Replace '(1 1) (Ref '(1 1 1)))
        (Replace '(2) (Term '* (list (Ref '(2)) (Ref '(1 1 2 1)))))))

(define (check-prog prog exs)
  (define p (if (procedure? prog) prog (curry interpret prog)))
  (for ([ex exs])
    (assert (equal? (p (car ex)) (cdr ex)))))

(define (checks)
  (check-prog co-prog co-ex)
  (check-prog clt-prog clt-ex)
  (check-prog mf-prog mf-ex))

; ------------- Synthesis queries ------------- ;

; A macro that runs each synthesis query in isolation and 
; discards all global state that is normally kepth by Rosette 
; after the query terminates.
(define-syntax-rule (synth* problem sketch examples)
  (parameterize ([term-cache (hash-copy (term-cache))])
    (printf "----- Synthesizing ~a -----\n" problem)
    (define sk (time sketch))
    (define sol (time (synth sk examples)))
    (cond [(sat? sol) (printf "Synthesized: ~a\n" (evaluate sk sol))]
          [else (printf "No solution found.\n")])))

; Synthesizing programs with len actions, with expressions of depth 0-depth.
(define (workload [depth 2] [len 2])
  (synth* 'co  (sketch '((1) (2)) '(+) depth len) co-ex)
  (synth* 'clt (sketch '((2) (1 1) (2 1)) '(+ *) depth len) clt-ex)
  (synth* 'mf  (sketch '((2) (1 1) (1 1 1) (1 1 2 1)) '(+ * =) depth len) mf-ex))
