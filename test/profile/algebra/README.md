# A Toy RuleSynth Benchmark

This example comes from the RuleSynth project. It shows two ways to
implement action synthesis for the domain of K-12 algebra, where the
goal is to synthesize rules for solving simple algebraic equations:

- `slow.rkt` contains the original (slow) implementation, in which
the main bottleneck is symbolic evaluation.
- `fast.rkt` contains the optimized implementation, which uses a
combination of symbolic reflection and code rewriting to avoid the use
of mutiple return values (which de-correlate paths, causing blowup).

