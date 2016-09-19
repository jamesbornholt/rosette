#lang racket/base

(require racket/cmdline
         raco/command-name
         profile/raco-utils
         "compile.rkt" "tool.rkt"
         "renderer/complexity.rkt" "renderer/summary.rkt" "renderer/trace.rkt"
         "renderer/html.rkt")

;; raco symprofile (based on raco feature-profile)
;; profile the main submodule (if there is one), or the top-level module

(define plot-graphs? (make-parameter #f))
(define profile-mode (make-parameter 'complexity))
(define file
  (command-line #:program (short-program+command-name)
                #:once-any
                ["--complexity" "Produce a complexity profile (the default)"
                                (profile-mode 'complexity)]
                ["--summary" "Produce a simple summary profile"
                             (profile-mode 'summary)]
                ["--trace" "Produce a complete execution trace"
                           (profile-mode 'trace)]
                ["--html" "Produce an interactive HTML profile"
                          (profile-mode 'html)]
                #:once-each
                [("-g" "--graph") "Plot graphs when available"
                                 (plot-graphs? #t)]
                #:args (filename . rest)
                ; pass all unused arguments to the file being run
                (current-command-line-arguments (list->vector rest))
                filename))

(define renderer
  (case (profile-mode)
    ['complexity (complexity-renderer #:plot? (plot-graphs?))]
    ['summary    (summary-renderer)]
    ['trace      (trace-renderer)]
    ['html       (html-renderer)]))

(collect-garbage)
(collect-garbage)
(collect-garbage)

(current-compile symbolic-profile-compile-handler)
(profile-thunk
  (lambda ()
    (dynamic-require (module-to-profile file) #f))
  #:source (format "file ~a" file)
  #:renderer renderer)
