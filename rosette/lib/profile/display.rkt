#lang racket

(require "record.rkt")
(provide (all-defined-out))

;; Displays a profile entry.
(define (display-profile node [level 0])
  (match-define (profile-node _ proc _ _ _ real _ children) node)
  (define indent (string-join (for/list ([i level]) "  ") ""))
  (printf "~a* ~a (~v msec)\n" indent proc real)  ;├─
  (for ([c children]) (display-profile c (add1 level))))