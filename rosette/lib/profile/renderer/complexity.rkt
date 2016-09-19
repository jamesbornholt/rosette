#lang racket

(require plot "../record.rkt" "../feature.rkt" "stats.rkt" "key.rkt")

(provide complexity-renderer)

(define (feature->feature feature)
  (lambda (node) (hash-ref (profile-node-inputs node) feature #f)))

(define (feature->metric feature)
  (procedure-rename
   (lambda (node) (hash-ref (profile-node-outputs node) feature #f))
   (feature-name feature)))

(define (name->metric name)
  (procedure-rename
   (lambda (node) (hash-ref (profile-node-metrics node) name #f))
   name))

(define metrics (map name->metric '(merge-count term-count union-count union-size real)))

; Create a renderer that outputs information about the complexity of
; procedures observed in a profile. If plot? is true, the renderer will
; also produce a plot in a new window.
(define (complexity-renderer #:plot? [plot? #f])
  (lambda (profile source)
    (unless (profile-node? profile)
      (raise-argument-error 'complexity-renderer "profile-node?" profile))
    (printf "=== Complexity profile (source: ~v) ===\n" source)
    (for* ([feature (current-features)]
           [metric (cons (feature->metric feature) metrics)])
      (let ([result (analyze-complexity profile (feature->feature feature) metric)])
        (render-complexity/text result feature metric)
        (when plot?
          (render-complexity/plot result feature metric))))))


; Represents a power-law fit to the complexity of a single procedure:
;   output = a*input^2 with goodness of fit R2
; corresponding to the raw data points listed in x and y.
; If a and b are #f, no fit coould be computed for the function.
(struct procedure-complexity (proc a b R2 x y) #:transparent)


; Analyze the complexity of all procedures in the given profile with respect
; to a single feature and metric.
(define (analyze-complexity profile feature metric [key profile-node-key/srcloc])
  ; functions will map proc -> (listof pair?) -- the set of all points to
  ; consider for each procedure.
  (define functions (make-hash))
  (let rec ([node profile])
    (match-let ([proc (key node)]
                [x (feature node)] 
                [y (metric node)]) 
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
(define (render-complexity/text nodes feature metric)
  (define (~f x [p 2]) (~r x #:precision p))
  (define fname (feature-name feature))
  (define mname (object-name metric))
  (define nodes* (sort (filter (lambda (n) (not (false? (procedure-complexity-a n)))) nodes)
                       > #:key procedure-complexity-b))
  (unless (null? nodes*)
    (printf "--- Complexity profile for (~a, ~a) ---\n" fname mname)
    (for ([n nodes*])
      (match-define (procedure-complexity proc a b R2 x y) n)
      (printf "~a: output ~a = ~a * (input ~a)^~a (R^2 = ~a)\n"
              proc mname (~f a) fname (~f b) (~f R2 3)))
    (printf "\n")))

; Take a list of procedure-complexity? instances and render them as a graph.
(define (render-complexity/plot nodes feature metric)
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
      (plot renderers #:title (format "(~v, ~v)" (feature-name feature) (object-name metric))))))
