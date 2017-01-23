#lang rosette 

(require rosette/lib/angelic)


;; A tree is represented as nested lists, with accessor functions defined below:
(define node-parent-child car)    ; The name of this child node with respect to
                                  ; its parent, 'root' for the root node.
                                  ; Eg. child1, child2, etc

(define node-class        cadr)   ; The class that this node belongs to. This
                                  ; determines the type of attributes and
                                  ; children the node has

(define node-attributes   caddr)  ; An associative list mapping attribute names
                                  ; to a bit indicating whether they have been
                                  ; assigned.

(define node-children     cadddr) ; A list of the children of this node. May be
                                  ; empty for leaf nodes

(define (get-attribute n attr-name)
  (unbox (cdr (assoc attr-name (node-attributes n)))))
(define (get-child n child)
  (if (null? child)
      n
      (assoc child (node-children n))))

(define (set-attribute! n attr-name val)
  (map (lambda (p) (if (equal? attr-name (car p))
                       (set-box! (cdr p) val)
                       (void)))
       (node-attributes n))
  (void))

; An attribute is represented by a list of symbols, containing:
(define assign-class car)   ; The class of the attribute
(define assign-child cadr)  ; The child name of the class, self if it is '()
(define assign-name  caddr) ; The name of the actual attribute

;; The example grammar:
;;
;;   interface Top {}
;;   interface Node {
;;     var a: int ;
;;     var b: int ;
;;   }
;;
;;   class Root: Top {
;;     children {
;;       child: Node;
;;     }
;;
;;     actions{
;;       child.a := child.b;
;;     }
;;   }
;;
;;   class MidNode: Node {
;;     children {
;;       left:  Node;
;;       right: Node;
;;     }
;;     actions {
;;       left.a  := a;
;;       right.a := a;
;;       b       := left.b + right.b;
;;     }
;;   }
;;
;;   class Leaf: Node {
;;     actions {
;;       b := 0;
;;     }
;;   }

;; Here are macros to facilitate the construction of a tree
(define (eg-attrs) `[(a . ,(box #f)) (b . ,(box #f))])

(define (eg-leaf1) `(left leaf ,(eg-attrs) []))

(define (eg-leaf2) `(right leaf ,(eg-attrs) []))

(define-syntax-rule (leaf name)
  `(name leaf ,(eg-attrs) []))

(define-syntax-rule (midnode name child1 child2)
  `(name midnode ,(eg-attrs) [,child1 ,child2]))

(define-syntax-rule (root name child)
  `(name root [] [,child]))

(define (eg-tree)
  (root top (midnode child
                     (leaf left)
                     (leaf right))))

;; The dependencies of our example schedule, endcoded as an associative list,
;; with the first element being the attribute assigned, the rest are its
;; dependencies.
(define eg-deps
  '([(root child a)    (root child b)]
    [(midnode left a)  (midnode () a)]
    [(midnode right a) (midnode () a)]
    [(midnode () b)    (midnode left b) (midnode right b)]
    [(leaf () b)]))

;; One valid schedule for the grammar, BU is bottom-up, TD is top-down, followed
;; by an ordered list of attributes to be assigned on that traversal.
;;
(define eg-schedule
  '([BU (leaf () b)
        (midnode () b)]
    [TD (root child a)
        (midnode left a)
        (midnode right a)]))

;; This schedule is invalid for general trees, but is valid for the example tree
;; defined above. This is because the tree above is too short and the second BU
;; traversal has the same effect as a TD one.
(define eg-invalid-schedule
  '((BU (midnode () b)
        (root child a)
        (leaf () b))
    (BU (midnode left a)
        (midnode right a))))

;; Checks that a specific schedule is valid on a specific tree. This is to be
;; used later for synthesis and verification. It performs traversals specified
;; by the schedule on the tree, setting attributes after checking that all their
;; dependencies have been satisfied.
(define (check-schedule sched tree)
  (map (lambda (traversal)
         (let ([type    (car traversal)]
               [assigns (cdr traversal)])
           (cond [(equal? type 'TD) (check-td assigns tree)]
                 [(equal? type 'BU) (check-bu assigns tree)])))
       sched)
  tree)

(define (check-td assigns tree)
  (unless (null? tree)
    (check-assigns assigns tree)
    (map (lambda (child) (check-td assigns child))
         (node-children tree))))

(define (check-bu assigns tree)
  (unless (null? tree)
    (map (lambda (child) (check-bu assigns child))
         (node-children tree))
    (check-assigns assigns tree)))

;; Main function that checks the dependencies for one assignment. Also checks
;; another constraint: every attribute is assigned to once.
(define (check-assigns assigns tree)
  (let ([cur-assigns (filter (lambda (assign)
                               (equal? (assign-class assign)
                                       (node-class tree)))
                             assigns)])
    ;(displayln cur-assigns)
    ;(displayln tree)
    (map (lambda (assign)
           (assert (not (attr-value tree assign))) ; Single assignment
           (check-deps assign tree)
           (set-attribute! (get-child tree (assign-child assign))
                           (assign-name assign)
                           #t))
         cur-assigns)))

(define (check-deps assign tree)
  (unless (equal? assign '(leaf () no-op))
    (map (lambda (dep)
           (assert (attr-value tree dep)))
         (cdr (assoc assign eg-deps)))))

(define (attr-value tree attr)
  (get-attribute (get-child tree
                            (assign-child attr))
                 (assign-name attr)))

;; check-schedule would pass for empty schedules since there are no conflicts as
;; nothing is being assigned to. This function checks that all the attributes on
;; the tree have been assigned after the traversals.
(define (fully-assigned tree)
  (unless (null? tree)
    (map (lambda (attr)
           (assert (unbox (cdr attr))))
         (node-attributes tree))
    (map fully-assigned
         (node-children tree))
    (void)))

;; Generate a schedule sketch.
(define (make-sketch attrs [bound (length attrs)])
  (define alen (length attrs))
  
  (define-symbolic* sched-len integer?)
  
  (take 
   (for/list ([b bound])
     (define-symbolic* attr-len integer?)
     (cons (choose* 'TD 'BU)
           (take (for/list ([i alen]) (apply choose* attrs))
                 attr-len)))
   sched-len))








;; Procedure for creating a tree of depth <= d,
;; according to the grammar defined above.
(define (make-tree name depth)
  (assert (>= depth 0))
  (define-symbolic* unroll? boolean?)
  (if unroll?
      (local [(define-symbolic* leaf? boolean?)]
        (if leaf?
            `(,name leaf ,(eg-attrs) [])
            `(,name midnode ,(eg-attrs) 
                    [,(make-tree 'left (sub1 depth)) 
                     ,(make-tree 'right (sub1 depth))])))
      (assert #f)))

(define sketch
  (make-sketch '((root child a)
                 (midnode left a)
                 (midnode right a)
                 (midnode () b)
                 (leaf () b))))

;; Synthesizes a schedule which works on the specific example tree
(evaluate
 sketch
 (solve
  (begin
    (define t (eg-tree))
    (check-schedule sketch t)
    (fully-assigned t))))

;; Invalid schedule works for our example tree
(check-schedule eg-invalid-schedule (eg-tree))

;; We want to find a tree that does not satisfy our invalid schedule
(define t (root top (make-tree 'child  2)))
(define model (verify (check-schedule eg-invalid-schedule t)))
(evaluate t model)
