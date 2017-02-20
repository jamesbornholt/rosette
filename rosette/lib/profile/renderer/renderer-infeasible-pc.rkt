#lang racket/base

(provide gen:renderer/infeasible-pc
         renderer/infeasible-pc?
         finish-renderer/infeasible-pc
         )

(require racket/generic)

;; A structure that implements this generic interface should also implement the
;; start-renderer method of the gen:renderer.

(define-generics renderer/infeasible-pc
  ;; finish-renderer/infeasible-pc :
  ;; Renderer/InfeasiblePC ProfileState InfeasiblePCInfo -> Void
  (finish-renderer/infeasible-pc renderer/infeasible-pc
                                 profile
                                 infeasible-pc-info))
