#lang racket

(require racket/generic)
(provide (all-defined-out))

(define-generics renderer
  (start-renderer renderer profile reporter)
  (done-running renderer)
  (finish-renderer renderer profile))
