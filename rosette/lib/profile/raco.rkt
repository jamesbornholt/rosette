#lang racket/base

(require racket/cmdline
         raco/command-name
         profile/raco-utils
         "compile.rkt")

(define file (command-line #:program (short-program+command-name)
                           #:args (filename)
                           filename))

(collect-garbage)
(collect-garbage)
(collect-garbage)

(printf "hello world!\n")

(current-compile symbolic-profile-compile-handler)
(dynamic-require (module-to-profile file) #f)
