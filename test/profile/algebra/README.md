# A Toy RuleSynth Benchmark

This example comes from the RuleSynth project. It shows two ways to
implement action synthesis for the domain of K-12 algebra, where the
goal is to synthesize rules for solving simple algebraic equations.  The files `slow.rkt` and `fast.rkt` differ in their implementation of the `list-set` and `remove-at` procedures:

- `slow.rkt` contains the original (slow) implementation, in which
the main bottleneck is symbolic evaluation.
- `fast.rkt` contains the optimized implementation, which uses a
combination of symbolic reflection and code rewriting to avoid the use
of mutiple return values (which de-correlate paths).

In this particular benchmark, there is no difference between the slow and fast implementations.  The end-to-end behavior is the same, since the example use cases are not sufficiently complex to stress the `list-set` and `remove-at` procedures.

The file `bench.rkt` imports both of these implemetations, and tests them on an artificial workload that demonstrates the difference in performance.