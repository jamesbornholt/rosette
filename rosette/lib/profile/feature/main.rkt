#lang racket

(require compiler/compile-file
         "rewrite.rkt")

(provide symbolic-profile-compile-handler)

(define symbolic-profile-compile-handler
  (make-symbolic-profile-compile-handler))