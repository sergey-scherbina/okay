## handlers-vs-plain-loop - the handler numbers now have a plain-loop baseline

- `StagedBenchmark` has two new lanes over the same 1 000 State+Writer
  operations: `plainLoopVector`, a `while` loop with a `var` state and
  the same persistent `Vector` log the handlers thread, and
  `plainLoopBuffer`, the same loop with a mutable `ArrayBuffer`. A trial
  setup checks that every lane computes the same answer, state and log.
- Measured, minima of three quiet rounds: the plain loop 0.706 µs
  (buffer) and 2.79 µs (Vector); the staged block 7.79 µs, 2.79x the
  Vector loop; the Free block 14.2 µs, 5.10x.
- docs/benchmarks.md §2c states what this means: effect handlers in
  okay are not goto speed, and the staged road is the closest a JVM
  library gets to a compiled handler. specs/direct-staged.md Results
  has the numbers.
