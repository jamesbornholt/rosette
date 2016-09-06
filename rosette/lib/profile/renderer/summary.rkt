#lang racket

(require "../record.rkt" "key.rkt")

(provide summary-renderer)

; Create a renderer that aggregates inclusive and exclusive time
; per procedure and prints the results.
(define (summary-renderer)
  (lambda (profile)
    (unless (profile-node? profile)
      (raise-argument-error 'profile-renderer "profile-node?" profile))
    (render-profile (aggregate-profile profile))))


; A profile entry is a single procedure together with inclusive and
; exclusive time.
(struct summary-entry (proc incl excl) #:transparent)


; Compute inclusive and exclusive times for each procedure observed
; in the given profile.
(define (aggregate-profile profile [key profile-node-key/srcloc])
  (define excl (make-hash))
  ; Compute exclusive time with a bottom-up traversal
  (let rec ([node profile])
    (define inct (profile-node-real node))
    (define in-child (apply + (map rec (profile-node-children node))))
    (unless (false? inct)
      (hash-update! excl (key node) (lambda (t) (+ t (- inct in-child))) 0))
    inct)
  ; Compute inclusive time with a top-down traversal
  (define incl (make-hash))
  (let rec ([node profile][stack (set)])
    (define inct (profile-node-real node))
    (unless (or (false? inct) (set-member? stack (key node)))
      (hash-update! incl (key node) (lambda (t) (+ t inct)) 0))
    (for ([c (profile-node-children node)]) (rec c (set-add stack (key node)))))
  ; Compute a profile
  (define prof
    (for/list ([proc (hash-keys excl)])
      (summary-entry proc (hash-ref incl proc) (hash-ref excl proc))))
  (sort prof > #:key summary-entry-excl))


; Render a profile to current-output-port
(define (render-profile profile)
  (unless (null? profile)
    (printf "--- Summary Profile (inclusive time/exclusive time) ---\n")
    (for ([node profile])
      (match-define (summary-entry proc incl excl) node)
      (printf "~a : ~vms / ~vms\n" proc incl excl))
    (printf "\n\n")))
