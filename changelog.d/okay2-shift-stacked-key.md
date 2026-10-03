## okay2-shift-stacked-key - okay2's keyed reset nests without a stack, on Scala.js too

The Scala 3 core's shift-stacked-key, ported to okay2 (operator: "Port
shift-stacked-key to okay2").

**A machine run outermost is a value.** `Shift.run`, and so every door
run outermost (the keyed `reset` included), now answers
`Free.delay(Own(program))`. Any other interpreter forces it once, and it
runs its own machine. A running machine that meets an `Own`, alone or as
a bind's left, steps into its program in its own loop, through
`resumeOwn`, a `Free.resume` that stops there. Nested resets are
therefore one machine's loop.

The keyed reset's per-thread room is gone: the `ThreadLocal` count and
`runReset`, which switched stacks past the room.

Results:
- 100 000 nested resets on a 128 KB JVM stack, with ZERO stack
  switches. Red first: the room switched once and then ran on the 1 GB
  fresh stack. That is why the test asserts zero switches rather than
  the core's "fewer than ten".
- 100 000 nested resets on Scala.js and Native (TestResetDepth, cross).
- A reset runs nothing until it is forced, and runs again each time.
  Red first: it was eager.

One claim (a cast), in one function (`stepInto`), with a comment saying
why the type is right.

Measured: `HandlerBenchmark.delimShift` 1.00x in two alternating rounds
(122.05 vs 122.30, 120.22 vs 120.70 µs), +32 B/op for the `Delay` and the
`Own`. The first shape tested for an `Own` before every rotation and read
1.02-1.04x; it is recorded as discarded in history.d.

Tests: TestResetDepth, TestResetSmallStack; okay2's whole suite (3356,
all platforms). Spec: specs/shift-effect.md.
