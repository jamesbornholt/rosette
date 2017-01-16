#lang rosette


(require (only-in racket/runtime-path define-runtime-path))
(require "../dom.rkt" "../websynthlib.rkt" "../websynth.rkt")

(require "../../bench.rkt")

(define-runtime-path html (build-path ".." "data/hello.html"))
(define dom (read-DOMNode html))
(define-tags (tags dom))

(define max_zpath_depth (depth dom))

(define-symbolic r0f0zpath tag? [max_zpath_depth])

(current-bitwidth #f)

(define (demonstration)
  (bench
   (assert (member "World" (zpath-interpret r0f0zpath dom)))
   (assert (path? r0f0zpath dom "World")))
)


(define (scrape)
  (define sol (solve (demonstration)))

  (define r0f0zpath_list (map label (evaluate r0f0zpath sol)))
  (define field0_zpath (synthsis_solution->zpath r0f0zpath_list))

  (zip (DOM-Flatten (DOM-XPath dom field0_zpath))))

(profile-bench! "slow hello_1" (with-variant 0 (scrape)))
(profile-bench! "fast hello_1" (with-variant 1 (scrape)))