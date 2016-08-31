#lang racket

(provide least-squares/powerlaw r-squared)

; Fit a power law of the form y = ax^b to a list of points.
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

; Compute the R-squred goodness of fit measure for a model, expressed as a
; procedure f(x).
(define (r-squared points model)
  (define xs (map car points))
  (define ys (map cdr points))
  (define n (length points))
  (define es (for/list ([x xs][y ys]) (- y (model x))))  ; residuals
  (define ss_res (apply + (for/list ([e es]) (* e e))))
  (define mean (/ (apply + ys) (exact->inexact n)))
  (define ss_tot (apply + (for/list ([y ys]) (* (- y mean) (- y mean)))))
  (if (= ss_tot 0) 1 (- 1 (/ ss_res ss_tot))))
