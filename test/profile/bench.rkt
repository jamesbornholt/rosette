#lang racket

(require "bench/config.rkt" "bench/form.rkt" "bench/struct.rkt" "bench/profile.rkt")

(provide merge-structs? variant with-variant
         bench bench-struct 
         profile-bench profile-bench!)