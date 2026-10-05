## cont-js-depth stage 4 — the strict `k` by re-execution: no stack overflow on Scala.js

A shift body that USES its strict `k`'s answer (`k => k(1) + 1`) nests
the host stack, and Scala.js had nothing to switch to: it overflowed
between 300 and 1 000 levels. Now, at the end of a room (64 strict
levels), the innermost body's `k` throws a suspension
(`Delimited.Suspend`) that unwinds to the run's driver. Every strict
body on the way records itself: its continuation and its `k`'s answers
so far. The driver runs the deepest `k` on the shallow stack, then the
recorded bodies again, innermost first, each `k` answering from memory.
This is linear: every level runs again at most once. A million nested
bodies now run on Scala.js in ~2.3 s (red first: "Maximum call stack
size exceeded").

The contract: where a suspension crosses a strict body, the body's part
before its pending `k` call runs again (docs/cont-stack.md). A `k`
called from anywhere but its own body is a barrier with a driver of its
own. On by default on Scala.js; `-Dokay.cont.replay=true` on the JVM and
Native, which keep the fresh stack by default. There re-execution costs
4.94x on 1 000 opaque levels, and on Native a throw unwinds at ~55 µs a
frame. Off, it costs nothing (1.00x / 0.99x / 1.01x). The JVM default
is the operator's call (backlog cont-replay-jvm-default).

Spec: specs/cont-js-depth.md, stage 4. Tests: TestContReplay,
TestContReplaySmallStack.
