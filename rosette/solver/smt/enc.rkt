#lang racket

(require (prefix-in smt/ (only-in "smtlib2.rkt" bv not and or xor => <=> ite = <))
         (except-in "smtlib2.rkt" bv not and or xor => <=> ite = <) 
         "env.rkt" "../common/enc.rkt" 
         (only-in "../../base/core/term.rkt" expression expression? constant? get-type type-of)
         (only-in "../../base/core/polymorphic.rkt" ite =?)
         (only-in "../../base/core/bool.rkt" ! && || => <=>)
         (only-in "../../base/core/num.rkt" 
                  current-bitwidth 
                  @= @< @<= @> @>= @+ @* @*h @- @/ @abs @sgn @quotient @remainder @expt                   
                  @<< @>> @>>> @bitwise-not @bitwise-and @bitwise-ior @bitwise-xor)
         (only-in "../../base/core/bitvector.rkt" 
                  bitvector? bv bitvector-size 
                  @bveq @bvslt @bvsle @bvult @bvule   
                  @bvnot @bvor @bvand @bvxor @bvshl @bvlshr @bvashr
                  @bvneg @bvadd @bvmul @bvudiv @bvsdiv @bvurem @bvsrem @bvsmod
                  @concat @extract @zero-extend @sign-extend @int->bv @bv->int @bv->nat)
         (only-in "../../base/struct/enum.rkt" enum-literal? ordinal enum-size))

(provide enc finitize)

; The enc procedure takes a value and an environment, and returns  
; an SMTLIB identifier representing that value in the given environment.  If it 
; cannot produce an encoding for the given value, an error is thrown. 
; The environment will be modified (if needed) to include an encoding for 
; the given value and all of its subexpressions (if any).
(define (enc v env)
  (ref! env v (match v
                [(? expression?) (enc-expr v env)]
                [(? constant?)  (enc-const v env)]
                [_             (enc-lit v env)])))

(define (enc-expr v env)
  (match v
    [(expression (== @*) -1 e) 
     (bvneg (enc e env))]
    [(expression (== @*) (and (? rational?) (not (? integer?)) r) es ...) 
     (bvsdiv (apply bvmul (enc (numerator r) env) (for/list ([e es]) (enc e env))) 
             (enc (denominator r) env))]
    [(expression (== @*) a ... (expression (== @expt) x (and (? number?) (? negative? n))) b ...)
     (let ([a/x^n (bvsdiv (enc (apply @* a) env) (enc (@expt x (- n)) env))])
       (if (null? b) a/x^n (bvmul a/x^n (enc (apply @* b) env))))]
    [(expression (== @expt) e (? integer? n)) 
     (let ([e^n (apply bvmul (make-list (abs n) (enc e env)))])
       (if (< n 0) (bvsdiv 1 e^n) e^n))]
    [(expression (app rosette->smt (? procedure? smt/op)) es ...)
     (apply smt/op (for/list ([e es]) (enc e env)))]
    [(or (expression (and op (== @int->bv)) v (app bitvector-size sz))
         (expression (and op (== @bv->int)) (and v (app get-type (app bitvector-size sz))))
         (expression (and op (== @bv->nat)) (and v (app get-type (app bitvector-size sz)))))
     (let-values ([(src tgt) (if (equal? op @int->bv) (values (current-bitwidth) sz) (values sz (current-bitwidth)))]
                  [(extend)  (if (equal? op @bv->nat) zero_extend sign_extend)])
       (cond [(= src tgt) (enc v env)]
             [(> src tgt) (extract (- tgt 1) 0 (enc v env))]
             [else        (extend (- tgt src) (enc v env))]))]
    [(expression (== @extract) i j e)
     (extract i j (enc e env))]
    [(expression (== @zero-extend) v t)
     (zero_extend (- (bitvector-size t) (bitvector-size (get-type v))) (enc v env))]
    [(expression (== @sign-extend) v t)
     (sign_extend (- (bitvector-size t) (bitvector-size (get-type v))) (enc v env))]
    [(expression (== @*h) x y)
     (extract (sub1 (* 2 (current-bitwidth))) 
              (current-bitwidth)
              (bvmul (concat (enc 0 env) (enc x env)) 
                     (concat (enc 0 env) (enc y env))))]
    [_ (error 'encode "cannot encode expression ~a" v)]))

(define (enc-const v env)
  (ref! env v))

(define (enc-lit v env)
  (match v 
    [#t true]
    [#f false]
    [(? number?) (smt/bv (finitize v) (current-bitwidth))]
    [(bv lit t) (smt/bv lit (bitvector-size t))]
    [(? enum-literal?) (smt/bv (ordinal v) (integer-length (enum-size (type-of v))))]
    [_ (error 'enc-literal "expected a boolean?, number? or enum-literal?, given ~a" v)]))

(define-encoder rosette->smt 
  [#:== ; bool 
        [! smt/not] [&& smt/and] [|| smt/or] [=> smt/=>] [<=> smt/<=>]  
        ; num
        [ite smt/ite] [=? smt/=] [@= smt/=] [@< bvslt] [@<= bvsle] 
        [@bitwise-ior bvor] [@bitwise-and bvand] 
        [@bitwise-not bvnot] [@bitwise-xor bvxor]
        [@<< bvshl] [@>>> bvlshr] [@>> bvashr]
        [@+ bvadd] [@* bvmul] [@quotient bvsdiv] [@remainder bvsrem]
        [@abs smt/abs] [@sgn smt/sgn]
        ; bitvector
        [@bveq smt/=] [@bvslt bvslt] [@bvsle bvsle] [@bvult bvult] [@bvule bvule] 
        [@bvnot bvnot] [@bvor bvor] [@bvand bvand] [@bvxor bvxor] 
        [@bvshl bvshl] [@bvlshr bvlshr] [@bvashr bvashr]
        [@bvneg bvneg] [@bvadd bvadd] [@bvmul bvmul] [@bvudiv bvudiv] [@bvsdiv bvsdiv]
        [@bvurem bvurem] [@bvsrem bvsrem] [@bvsmod bvsmod] [@concat concat]]
  [#:?  [enum-comparison-op? bvult]])

(define (smt/abs e)
  (smt/ite (bvslt e (smt/bv 0 (current-bitwidth))) (bvneg e) e))

(define (smt/sgn e)
  (let ([zero (smt/bv 0 (current-bitwidth))]) 
    (smt/ite (smt/= e zero) zero 
             (smt/ite (bvslt e zero) 
                      (smt/bv -1 (current-bitwidth))
                      (smt/bv  1 (current-bitwidth))))))
