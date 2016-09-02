#lang racket

(require plot
         "../record.rkt" "../feature.rkt" "stats.rkt")

(provide complexity-renderer)


; Create a renderer that outputs information about the complexity of
; procedures observed in a profile. If plot? is true, the renderer will
; also produce a plot in a new window.
(define (complexity-renderer #:plot? [plot? #f])
  (lambda (profile)
    (unless (profile-node? profile)
      (raise-argument-error 'complexity-renderer "profile-node?" profile))
    (for ([feature (current-features)])
      (let ([result (analyze-complexity profile feature)])
        (render-complexity/text result feature)
        (when plot?
          (render-complexity/plot result feature))))))


; Represents a power-law fit to the complexity of a single procedure:
;   output = a*input^2 with goodness of fit R2
; corresponding to the raw data points listed in x and y.
; If a and b are #f, no fit coould be computed for the function.
(struct procedure-complexity (proc a b R2 x y) #:transparent)


; Analyze the complexity of all procedures in the given profile with respect
; to a single feature.
(define (analyze-complexity profile feature)
  ; functions will map proc -> (listof pair?) -- the set of all points to
  ; consider for each procedure.
  (define functions (make-hash))
  (let rec ([node profile])
    (match-let ([proc (profile-node-procedure node)]
                [x (hash-ref (profile-node-inputs node) feature #f)]
                [y (hash-ref (profile-node-outputs node) feature #f)])
      (unless (or (false? x) (false? y))
        (hash-update! functions proc (lambda (old) (cons (cons x y) old)) '())))
    (for ([c (profile-node-children node)])
      (rec c)))
  (for/list ([(proc pts) functions])
    (define valid-pts (filter (lambda (pt) (and (> (car pt) 0) (> (cdr pt) 0))) pts))
    (define-values (a b r2)
      (cond [(<= (length valid-pts) 1)
             (values #f #f #f)]
            [else
             (let*-values ([(a b) (least-squares/powerlaw valid-pts)]
                           [(model) (lambda (x) (* a (expt x b)))]
                           [(r2) (r-squared valid-pts model)])
               (if (and (> (abs b) 0.1) (not (infinite? b)))
                   (values a b r2)
                   (values #f #f #f)))]))
    (procedure-complexity proc a b r2 (map car pts) (map cdr pts))))

; Take a list of procedure-complexity? instances and render them as text.
(define (render-complexity/text nodes feature)
  (define (~f x [p 2]) (~r x #:precision p))
  (define fname (feature-name feature))
  (define nodes* (sort (filter (lambda (n) (not (false? (procedure-complexity-a n)))) nodes)
                       > #:key procedure-complexity-b))
  (unless (null? nodes*)
    (printf "--- Complexity profile for ~a ---\n" fname)
    (for ([n nodes*])
      (match-define (procedure-complexity proc a b R2 x y) n)
      (printf "~a: output ~a = ~a * (input ~a)^~a (R^2 = ~a)\n"
              (object-name proc) fname (~f a) fname (~f b) (~f R2 3)))
    (printf "\n")))

; Take a list of procedure-complexity? instances and render them as a graph.
(define (render-complexity/plot nodes feature)
  (define renderers
    (for/fold ([ret '()])
              ([n (filter (lambda (n) (not (false? (procedure-complexity-a n)))) nodes)])
      (match-define (procedure-complexity proc a b R2 x y) n)
      (let ([color (/ (length ret) 2)])
        (append ret (list (function (lambda (x) (* a (expt x b)))
                                    #:color color #:label (~a (object-name proc)))
                          (points (map vector x y) #:color color))))))
  (unless (null? renderers)
    (parameterize ([plot-new-window? #t])
      (plot renderers #:title (~v (feature-name feature))))))
