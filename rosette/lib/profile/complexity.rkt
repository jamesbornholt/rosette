#lang racket

(require "record.rkt" rosette/base/core/term rosette/base/core/union plot)
(provide complexity)

(plot-new-window? #t)

; Define some trivial features. They must be strictly positive for power law
; regression to be correct.
(define (expr-depth e)
  (match e
    [(expression elts ...) (+ 1 (apply max (map expr-depth elts)))]
    [(union : (_ v) ...) (+ 1 (apply max (map expr-depth v)))]
    [(list elts ...) (if (null? elts) 1 (+ 1 (apply max (map expr-depth elts))))]
    [_ 1]))

(define (expr-length e)
  (match e
    [(expression elts ...) (+ 1 (apply + (map expr-length elts)))]
    [(union : (_ v) ...) (+ 1 (apply + (map expr-length v)))] ; TODO treat guards too
    [(list elts ...) (if (null? elts) 1 (+ 1 (apply + (map expr-length elts))))]
    [_ 1]))

(define (union-size e)
  (match e
    [(union contents) (length contents)]
    [_ #f]))


; Reducers for features over lists of arguments.
(define (make-argmax proc)
  (lambda (l) (let ([l* (filter number? (map proc l))])
                (if (null? l*) #f (apply max l*)))))

(define (make-argsum proc)
  (lambda (l) (let ([l* (filter number? (map proc l))])
                (if (null? l*) #f (apply + l*)))))


; Feature definitions.
(struct feature (proc name) #:transparent)
(define features
  (list (feature (make-argmax expr-depth) "expr depth")
        (feature (make-argmax expr-length) "expr length")
        (feature (make-argmax union-size) "union size")))


; Fit a power law to a list of (x y) points.
; The result is values (a b) such that y = ax^b is the best fit.
(define (least-squares/powerlaw points)
  (define xs (map car points))
  (define ys (map cdr points))
  (define ∑lnx (apply + (map log xs)))
  (define ∑lnx2 (apply + (for/list ([x xs]) (* (log x) (log x)))))
  (define ∑lny (apply + (map log ys)))
  (define ∑lnxlny (apply + (for/list ([x xs][y ys]) (* (log x) (log y)))))
  (define n (length points))
  (define b (/ (- (* n ∑lnxlny) (* ∑lnx ∑lny))
               (- (* n ∑lnx2) (* ∑lnx ∑lnx))))
  (define a (exp (/ (- ∑lny (* b ∑lnx)) n)))
  (values a b))

; Compute the R-squred goodness of fit measure for the given model
; (a procedure of one argument x that returns f(x)) against the given points.
(define (r-squared points model)
  (define xs (map car points))
  (define ys (map cdr points))
  (define n (length points))
  (define es (for/list ([x xs][y ys]) (- y (model x))))  ; residuals
  (define ss_res (apply + (for/list ([e es]) (* e e))))
  (define mean (/ (apply + ys) (exact->inexact n)))
  (define ss_tot (apply + (for/list ([y ys]) (* (- y mean) (- y mean)))))
  (if (= ss_tot 0) 1 (- 1 (/ ss_res ss_tot))))


; Compute the empirical complexity of a single feature for a profile.
(define (complexity/feature profile feat)
  (match-define (feature fproc fname) feat)
  ; Gather a list of points for each procedure
  (define (node->point node)
    (let ([x (fproc (profile-node-inputs node))]
          [y (fproc (profile-node-outputs node))])
      (if (and (number? x) (number? y)) (cons x y) #f)))
  (define functions (make-hash))
  (let rec ([node profile])
    (let ([pt (node->point node)])
      (when (pair? pt)
        (let ([old (hash-ref functions (profile-node-procedure node) '())])
          (hash-set! functions (profile-node-procedure node) (cons pt old)))))
    (for ([c (profile-node-children node)])
      (rec c)))

  ; Fit power law curves to each function
  (struct curve (x y a b label) #:transparent)
  (define curves
    (for/fold ([curves '()]) ([(proc pts) functions])
      (if (> (length pts) 1)
          (let ()
            (define-values (a b) (least-squares/powerlaw pts))
            (define (model x) (* a (expt x b)))
            (define r2 (r-squared pts model))
            (if (and (> (abs b) 0.1) (not (infinite? b)))
                (let ()
                  (printf "~v: ~v*(~a)^~v (R^2 = ~v)\n" proc a fname b r2)
                  (define lbl (format "~v/~v" (if (procedure? proc) (object-name proc) proc) fname))
                  (cons (curve (map car pts) (map cdr pts) a b lbl) curves))
                curves))
          curves)))

  ; Plot
  (define renderers
    (for/fold ([ret '()]) ([c curves])
      (match-define (curve x y a b lbl) c)
      (define col (/ (length ret) 2))
      (append ret
              (list (function (lambda (x) (* a (expt x b))) #:color col #:label lbl)
                    (points (map vector x y) #:color col)))))
  renderers)


; Compute fits for the complexity of each function in the profile
(define (complexity profile)
  (define rs
    (for/fold ([ret '()]) ([f features])
      (append ret (complexity/feature profile f))))
  (plot rs))
