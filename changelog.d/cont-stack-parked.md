## cont-stack-parked — a stack switch hands the rest to a parked, warm worker

specs/cont-stack.md plan stage D3, moved up by stage A's measurement
(history.d `cont-stack-ab`, the same evening): on the COUNT road — a
JVM without `--enable-native-access`, what a library user gets —
statePara's ~2 000 levels outrun the first room and switched once a
run to a NEW 1 GB thread, and that switch was the whole of its 5.03x
(134.8 vs 26.8 µs; 1.08 with the switch taken away). fib100's 1.17x is
the room bookkeeping, plan stage C; fib1000 1.01.

- `StackPool` (`src/main/scala-jvm-native`, one file for JVM and
  Native): `StackSwitch.fresh` takes an idle worker or makes one, hands
  it the segment and parks; the worker publishes the answer or the
  exception, unparks the caller and offers itself back — at most 2
  idle (`-Dokay.cont.idleWorkers`), gone after 30 s idle
  (`-Dokay.cont.idleMillis`). A worker is made with
  `inheritThreadLocals = false`; the caller's context class loader is
  set for the segment; the caller's wait is uninterruptible (the
  worker holds its own program's frames) with the interrupt restored.
- Spin-then-park on both ends, 50 µs (`-Dokay.cont.spinMicros`): with
  the pool alone ~22 µs a switch remained, the two OS wake-ups; a
  segment shorter than the window costs none.
- Measured (MIN of 3 alternating rounds, `jmh-lane.sh`, JDK 26, ref
  b4934c052): statePara 134.8 → 50.3 (pool) → 31.8 µs (spin) against
  27.6 — 5.03x → 1.88 → 1.15. fib100 unchanged (no switch there).
- TestStackPool (4, red first: five switching runs finished on five
  threads): reuse, 8 concurrent callers, an exception after the switch
  with the worker surviving it, the context class loader. Core 523,
  Native, JS, JDK 17 green.
- history.d: `cont-stack-ab` (stage A) and `cont-stack-parked`.
