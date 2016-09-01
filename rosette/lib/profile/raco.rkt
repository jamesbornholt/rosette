#lang racket/base

(require racket/cmdline
         raco/command-name
         profile/raco-utils
         "compile.rkt" "tool.rkt" "renderer/complexity.rkt")

;; raco symprofile (based on raco feature-profile)
;; profile the main submodule (if there is one), or the top-level module

(define plot-graphs? (make-parameter #f))
(define file
  (command-line #:program (short-program+command-name)
                #:once-each
                [("-p" "--plot") "Plot graphs when available"
                                 (plot-graphs? #t)]
                #:args (filename . rest)
                ; pass all unused arguments to the file being run
                (current-command-line-arguments (list->vector rest))
                filename))

(collect-garbage)
(collect-garbage)
(collect-garbage)

(current-compile symbolic-profile-compile-handler)
(profile-thunk
  (lambda ()
    (dynamic-require (module-to-profile file) #f))
  #:renderer (complexity-renderer #:plot? (plot-graphs?)))
