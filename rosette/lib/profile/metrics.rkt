#lang racket

(require (only-in rosette/base/core/merge merge-count)
         (only-in rosette/base/core/union union-count union-sum)
         (only-in rosette/base/core/term term-count))

; This module (re)-exports various metrics collected during symbolic evaluation, including:
; * (merge-count): the total number of merges performed since the beginning of execution.
; * (union-count): the total number of unions created since the beginning of execution.
; * (union-sum):   the sum of the cardinalities of all unions created since the beginning of execution.
; * (term-count):  the total number of terms created since the beginning of execution.

(provide merge-count union-count union-sum term-count)