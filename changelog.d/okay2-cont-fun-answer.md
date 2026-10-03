## okay2-cont-fun-answer - okay2's PState function answer is applied by a loop

The Scala 3 core's cont-fun-answer, ported to okay2 (operator:
"дальше?", option 1).

`PState.get`/`set` build their answer as a `PState.Bounce`: `next` gives
the rest of the program and `arg` its state, and `apply` is the loop.
Applying the answer of n steps therefore takes one host frame, not n.

Results:
- a million steps on a 128 KB thread, with the chain's end on that thread;
- a hundred thousand steps on JVM, JS and Native (TestPStateDepth, cross).
  Red first: StackOverflowError on both.

**Different from the core: one claim, in one function.** The loop
carries the pair (a function and its argument) erased:
- scalac 2 keeps no `@tailrec` across changing type arguments, which
  the core's typed loop relies on;
- the cast-free alternative is a typed holder each step, the 24 B an
  operation that cost the core 1.07x.

**Measured** on a new `statePara` lane in okay2's HandlerBenchmark,
alternated against master with the same lane:
- 0.93x and 0.94x (30.94 vs 33.19, 30.40 vs 32.51 µs);
- the same bytes. One closure applying the next is replaced by a loop
  that does not grow the stack.
