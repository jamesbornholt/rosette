#lang racket/base

(require racket/cmdline
         raco/command-name
         "compile.rkt" "tool.rkt"
         "renderer/complexity.rkt" "renderer/summary.rkt" "renderer/trace.rkt"
         "renderer/html.rkt")

;; raco symprofile (based on raco feature-profile)
;; profile the main submodule (if there is one), or the top-level module

(define plot-graphs? (make-parameter #f))
(define profile-mode (make-parameter 'complexity))
(define run-profiler? (make-parameter #t))
(define module-name (make-parameter 'main))
(define file
  (command-line #:program (short-program+command-name)
                #:once-any
                ; Profiler selections
                ["--complexity" "Produce a complexity profile (the default)"
                                (profile-mode 'complexity)]
                ["--summary" "Produce a simple summary profile"
                             (profile-mode 'summary)]
                ["--trace" "Produce a complete execution trace"
                           (profile-mode 'trace)]
                ["--html" "Produce an interactive HTML profile"
                          (profile-mode 'html)]
                #:once-each
                [("-l" "--compiler-only") 
                 "Only install the compile handler; do not run the profiler"
                 (run-profiler? #f)]
                [("-m" "--module") name
                 "Run submodule <name> (defaults to 'main)"
                 (module-name (string->symbol name))]
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

(define (run)
  (dynamic-require (module-to-profile file (module-name)) #f))

; check if there's a module of the given name, and if not,
; import the entire file instead
(define (module-to-profile file mod)
  (define file-path `(file ,file))
  (define main-path `(submod ,file-path ,mod))
  (dynamic-require file-path (void)) ; visit the module, but don't run it
  (if (module-declared? main-path #f)
      main-path
      file-path))

(if (run-profiler?)
    (profile-thunk run
                   #:source (format "file ~a" file)
                   #:renderer renderer)
    (run))
