#lang racket/base

(require racket/cmdline
         raco/command-name
         "compile.rkt"
         "tool.rkt"
         "renderer/complexity.rkt" 
         "renderer/summary.rkt" 
         "renderer/trace.rkt"
         "renderer/html.rkt"
         "renderer/stream.rkt"
         "renderer/noop.rkt")

;; raco symprofile (based on raco feature-profile)
;; profile the main submodule (if there is one), or the top-level module

(define renderer% (make-parameter make-html-renderer))
(define run-profiler? (make-parameter #t))
(define module-name (make-parameter 'main))
(define renderer-options (make-parameter (hash)))
(define file
  (command-line #:program (short-program+command-name)
                #:once-any  ; Profiler selections
                ["--complexity" "Produce a complexity profile (the default)"
                                (renderer% make-complexity-renderer)]
                ["--summary" "Produce a simple summary profile"
                             (renderer% make-summary-renderer)]
                ["--trace" "Produce a complete execution trace"
                           (renderer% make-trace-renderer)]
                ["--html" "Produce an interactive HTML profile"
                          (renderer% make-html-renderer)]
                ["--stream" "Run a streaming HTML profiler"
                            (renderer% make-stream-renderer)]
                ["--noop" "Produce no profile output (for testing)"
                          (renderer% make-noop-renderer)]
                #:once-each
                ; Tool configuration
                [("-l" "--compiler-only") 
                 "Only install the compile handler; do not run the profiler"
                 (run-profiler? #f)]
                [("-m" "--module") name
                 "Run submodule <name> (defaults to 'main)"
                 (module-name (string->symbol name))]
                ; Renderer-specific configuration
                [("-p" "--html-profile")
                 "HTML renderer: also open the Profile page"
                 (renderer-options (hash-set (renderer-options) 'html-profile #t))]
                [("-t" "--threshold") t
                 "Threshold percentage for pruning calls (a number in [0,1])"
                 (let ([th (string->number t)])
                   (when (or (eq? th #f) (< th 0) (> th 1))
                     (raise-argument-error 'threshold "number in [0,1]" t))
                   (renderer-options (hash-set (renderer-options) 'threshold th)))]
                [("-d" "--delay") d
                 "Streaming renderer: delay between samples, in seconds"
                 (let ([de (string->number d)])
                   (when (or (eq? de #f) (<= de 0))
                     (raise-argument-error 'delay "number > 0" d))
                   (renderer-options (hash-set (renderer-options) 'interval de)))]
                #:args (filename . rest)
                ; pass all unused arguments to the file being run
                (current-command-line-arguments (list->vector rest))
                filename))

; Set up the renderer
(define (renderer source-stx name)
  ((renderer%) source-stx name (renderer-options)))
(current-renderer renderer)

(collect-garbage)
(collect-garbage)
(collect-garbage)

(current-compile symbolic-profile-compile-handler)


; check if there's a module of the given name, and if not,
; import the entire file instead
(define (module-to-profile file mod)
  (define file-path `(file ,file))
  (define main-path `(submod ,file-path ,mod))
  (dynamic-require file-path (void)) ; visit the module, but don't run it
  (if (module-declared? main-path #f)
      main-path
      file-path))

(define mod (module-to-profile file (module-name)))

(define (run)
  (dynamic-require mod #f))


(if (run-profiler?)
    (profile-thunk run #:source (format "~a" mod)
                       #:name (format "~a" file))
    (run))
