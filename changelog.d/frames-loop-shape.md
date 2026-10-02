## frames-loop-shape - the frame machine's register pressure: inlining the continuation is not the cause; an array stack's premise priced

- `loop$1`'s register pressure (three loop registers kept in stack slots)
  is not relieved by keeping the user's continuation out of the loop:
  `-XX:CompileCommand=dontinline` on the benchmark's lambdas made
  contAnswer 1.18x SLOWER (30.7 -> 36.2 us) — inlining it pays more than
  its pressure costs.
- `FrameStoreBenchmark` prices a mutable array stack against the
  machine's linked `Frame` per bind: 1000 continuations pushed and popped
  in 0.91 us (fresh array) / 0.68 us (reused) against 2.85 us, 0-4 KB
  against 24 KB — G1's write barrier is not the cost. The array-stack
  machine waits on a typing decision (backlog frames-array-stack).
