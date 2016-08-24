#lang rosette

(provide merge-structs? variant bench-apply)

; Setting this parameter to #t (default) ensures
; that all value types defined with bench-struct
; are merged.  A bench-struct declaration specifies
; a value type iff the corresponding Rosette struct
; declaration specifies a value type.
(define merge-structs? (make-parameter #t))

; Setting this parameter to 0 (default) selects the
; default (first) expression in a bench form.
; Setting it to any other positive integer value selects
; the corresponding expression in a bench form.  If a
; given bench form has fewer expressions that the current
; value of code-variant, then the default (first) expression
; is used.
(define variant (make-parameter 0))

; This parameter stores a procedure that takes as input
; a procedure and a list of arguments.  It should apply
; the given procedure to the arguments and return the result,
; possibly also collecting statistics about the call.
(define bench-apply (make-parameter apply))