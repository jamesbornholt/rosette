# A Toy RuleSynth Benchmark

This example comes from the RuleSynth project. It shows two ways to
implement `list-set` and `remove-at` procedures, which are needed for implementing action synthesis for the domain of K-12 algebra.     

The file `actions.rkt` includes the fast and slow variants of these procedures.  The file `test.rkt` demonstrates the performance difference between the two.  Note that the difference is minimal for the application use cases (`example-tests:slow` and `example-tests:fast`), but it is significant for the artifical workloads designed to stress the two procedures.