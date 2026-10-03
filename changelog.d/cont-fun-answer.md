## cont-fun-answer - PState's function answer is applied by a loop: no host frame a step, on every platform

Operator: "cont-fun-answer". The last shape of the old
cont-stack-layer1-c list.

**What was wrong.** `PState.get` is `k => s => k(s)(s)`: the answer is a
function, and applying it applies the rest of the program inside its own
frame. A chain of n steps was therefore n host frames, OUTSIDE the
machine, where the JVM's stack switch cannot reach. A million steps
overflowed a 128 KB JVM thread, and a hundred thousand overflowed
Scala.js (red first, both: StackOverflowError).

**The fix is not the walked `Fun`/`Ap` that cost 2.8x in
cont-stack-layer1-b.** It is a trampolined APPLICATION:
- `get` and `set` build their answer as a `PState.Bounce`;
- its `next` gives the rest of the program, and `arg` gives the state
  for it; nothing is applied there;
- its `apply` is the loop, so whoever applies the answer gets it;
- a function that is not a `Bounce` (the final `ret`) ends the chain.

There is no cast: `Bounce` carries `Function1`'s variances, so the loop's
type test is implied by its scrutinee's type.

**Measured,** `HandlerBenchmark.statePara`, alternated, history.d:
- 1.00x in two rounds (62.43 vs 62.26, 62.78 vs 62.87 µs), with the same
  bytes;
- the first shape returned a `Next` pair each step and read 1.07x at
  +24 B an operation. It is recorded as discarded.

Tests: TestPStateSmallStack (a million steps on 128 KB, and the chain's
end on that thread), TestPStateDepth (cross: a hundred thousand steps; the
answer applied twice; `set` changing the state's type). Docs:
docs/cont-stack.md, a function answer. Specs: cont-stack.md and
cont-js-depth.md (the census answered); cont-strict-k's lead (2)
restated.
