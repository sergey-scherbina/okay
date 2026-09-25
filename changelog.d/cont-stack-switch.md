## cont-stack-switch — Cont past the stack: the room is read, not guessed, and the rest runs on a fresh thread

specs/cont-stack.md (stage 1c of stack-safety), Layers 2 and 3, on
every platform — landed by the operator's call before the macro layer
and before the A/B ("мерж в мастер", 2026-09-25; the number is backlog
cont-stack-ab).

- `Cont`'s runner carries the room left on this stack as a parameter
  of `step` and a field of `Reentry`, the continuation a body
  receives — no ThreadLocal. At zero it asks `StackSwitch.more`: the
  stack pointer and the thread's bounds decide a GRANT (one
  margin-sized slice at the worst bytes-a-level seen, kept in the
  run's `Gauge` behind the `Gauged` root of the continuation chain) or
  a SWITCH — `StackSwitch.fresh`, a 1 GB platform thread on every JDK
  (virtual threads refuted: 15–20x slower a level, humongous chunks
  cap a segment at 64). No exception unwinds, nothing runs twice.
- JVM: `StackRoom` root answers −1 (the count road: first room from
  `ThreadStackSize`, `-Dokay.cont.room`); `jdk22/StackRoom.scala` is
  its Multi-Release variant (`versioned("okayJdk22", …)`,
  `multiRelease` on the core) reading `getcontext`'s `sp` and pthread's
  bounds through FFM when `Module.isNativeAccessEnabled` — silently
  −1 otherwise; the floor includes HotSpot's guard+shadow zone (384 KB
  on macOS arm64: without it the runner overflowed 67 KB above the
  end, measured). macOS arm64 layout only; the others are backlog.
- Native: exact from the runtime's own `ThreadInfo` (its `stackTop` is
  the LOWEST address — the layout test caught the first cut) and a
  `stackalloc`; first room 64, a read there is a TLS access.
- JS: the written bound (the engine's stack), unchanged.
- Tests: TestContStack (10: 20 000 levels tail/answer-using/absorbed,
  multi-shot and an exception across switches, zero switches on 1000
  levels of a 2 MB thread, a 256 KB thread switches, 8 MB granted
  more than 2 MB), TestStackRoom (bounds on 26, −1 on 17),
  TestContStackNative (4); the core's tests fork with native access
  and `-Dokay.cont.room=64`, through the packaged jar. Green on JDK 26
  (exact), JDK 17 (count), Native, JS.
- Measured on the way: levels cost 1 504 B cold / 288 B warm; a whole
  room granted at the cold constant overflows (hence the slice); a
  platform-thread switch is 33 µs at any stack size, a virtual hop
  14 µs; `StackWalker` costs 7 µs before its first frame.
