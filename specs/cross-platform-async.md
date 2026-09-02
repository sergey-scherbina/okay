# Cross-platform Async

## Overview
The policy (verbatim intent): one and the same source runs on JVM, JS
and Native, using each platform's abilities transparently and fully,
losing nothing; and programs on different platforms interoperate — a
client on one, a server on another, the same code. Designed BEFORE the
cross-build lands so nothing has to be broken later.

## Design
- Programs stay in the effect world: `A ! Async` composes by flatMap —
  non-blocking by construction. Blocking exists only at the RUN
  boundary, and only on platforms that have it.
- `Async` gains a callback operation beside the thunk one:
  ```scala
  enum Async[+A]:
    case Run[A](run: () => A)                       // may block: JVM/Native
    case Await[A](register: (A => Unit) => Unit)    // universal
  ```
  Await is the cross-platform primitive (timers, I/O completions,
  Promise adapters); Run-with-blocking is a platform capability.
- Runners: JVM parks a Loom virtual thread; Native parks an OS thread
  (Scheduler.threads is the default there); JS drives the freer tree
  through the event loop — `runAsync(prog): Future[A]`, a different
  terminal, the SAME programs.
- Fiber: the cross-platform surface is completion + cancellation
  (onComplete / cancel / joinAsync). Blocking `join()` is
  evidence-gated (a `CanBlock` capability given only on JVM/Native),
  so misuse is a compile error on JS, not a runtime hang.
- Channel: ONE callback-based channel for all platforms
  (channel-callback, 2026-09-02) — src/main/scala/Channel.scala.
  `receive: Option[A] ! Async` and `send(a): Boolean ! Async` are
  programs; waiting lives in the channel's queues, never in a
  thread; the parking forms `receiveBlocking()`/`sendBlocking(a)`
  exist only under CanBlock, like Fiber.join. The state (buffer,
  waiting receivers, waiting senders with their elements) sits
  under a short lock, callbacks run outside it; on JS the lock is
  a no-op.
- Cross-platform interop between RUNNING programs (client here,
  server there) = okay-codec + a transport module; not Async's job.

## Behavior
- [x] one shared-source test suite passes on all three platforms
      (Await-based programs only) — src/test/scala-cross runs on JVM,
      on JS under Node, and on Native as a linked binary (12 tests
      each; okayNative/test in CI)
- [x] a JVM/Native-only test exercises blocking Run/join under the
      capability (TestAsync — the whole Loom suite runs under the
      CanBlock/Timer/Scheduler givens of scala-jvm/Platform.scala)
- [x] on JS, a sleep-then-answer program completes via runAsync
      without blocking the event loop (the cross suite asserts a
      timer interleaves with the sleep)
- [x] cancellation works on all platforms — at FIBER granularity:
      cancel interrupts the parked thread (JVM/Native) or stops the
      drive at its next operation (JS). Cancelling the Await
      REGISTRATION itself (unregistering the timer/IO completion)
      would need the register to answer with a canceller — open box
      below.

## Decisions
- **Await(register) over a Promise/Future-shaped op** — no dependency,
  no platform type in the core signature; adapters live at the edges.
- **Capabilities over subsetting**: JS does not get a crippled Fiber
  type; it gets the same type minus the evidence-gated methods —
  Fiber is onComplete/cancel, and join()(using CanBlock) is derived
  in the trait, so implementations only ever provide the callbacks.
- **Scheduler.fork takes the PROGRAM** (`() => A ! Async`), not a
  computed answer — that is what lets the event loop be a scheduler.
- **runAsync drives the tree in a while-loop** with an atomic
  handshake per Await (the callback may fire during registration, on
  any thread): whoever loses the exchange continues the drive.
- **Await carries the error channel and answers a canceller**:
  `register: (Either[Throwable, A] => Unit) => (() => Unit)`. The
  Left fails the whole program at that operation; the canceller
  unregisters (clearTimeout, an interrupt of the sleeping timer
  thread) and is invoked when a parked block is interrupted or a
  JS drive is cancelled while suspended. The simple top-level
  `await(k => ...)` keeps the success-only, nothing-to-unregister
  shape; the full form is `Async.await`.
- **par is cross-platform** — completion callbacks pair the answers,
  a child failure fails the pair through the error channel and
  cancels the sibling; `Fiber.joinAsync` is the effect-world join.
  race fails (with the later error) only when BOTH contenders fail —
  a lone failing contender still never wins.
- **send after close is REFUSED, not thrown** (channel-send-closed,
  2026-09-02): `send(a): Boolean` — true when the channel took the
  element, false once closed (the element dropped). The first cut
  had `send: Unit` with "do not send after close" as a comment:
  nothing enforced it, and a late send was either delivered as an
  ordinary element or lost without a trace. An exception was the
  obvious enforcement and the wrong one: a producer that outlives
  its stream is ordinary (a merge's second source, a remote feed
  whose consumer went away), and unwinding its fiber for that turns
  a fact into a fault. A Boolean is the fact — the producer reads it
  and stops. Exact under the race on the parking platforms: `send`
  checks open, puts, and re-checks; a put that landed after the
  close is taken back (`remove`), and counts as accepted only if a
  receiver already drained it — so "true" means received-or-buffered
  -before-the-end, and "false" means nobody will ever see it. The
  receiver's second poll after a closed check is the other half of
  the same race (an element put BEFORE the close, seen after the
  first poll). On JS one thread makes the check trivially exact.
  Under channel-callback the same contract holds by construction:
  every decision is one critical section, no take-back needed.
- **The channel waits in queues, not in threads** (channel-callback,
  2026-09-02). The parking channel (LinkedBlockingQueue) broke the
  stack's own rule — blocking must be GRANTED through CanBlock, and
  `receive()` parked without it — and it polled every 10ms to notice
  a close. On the JVM a parked virtual thread is cheap; on Native
  there is no Loom (the Scheduler there is one OS thread per fiber),
  so every waiting receiver was an OS thread asleep, and a
  fixed-size pool would deadlock on them. The JS design — a receiver
  leaves a callback, a sender leaves its element and a callback,
  close wakes them — costs nothing to wait and is exact under a
  short lock, so it became the only design. Consequences, stated: a
  suspended program continues on the thread that completed it (the
  sender's, at its send) — the JS behaviour, now everywhere; a
  ping-pong between two channels nests those continuations on one
  stack (the Drive's synchronous fast path bounds the common case);
  the parking forms are derived from the async ones the way
  Fiber.join is, and a JS call to them is a compile error naming
  CanBlock. Not done here: the Native Scheduler still forks an OS
  thread per fiber — a fixed pool is now safe to introduce (BACKLOG
  native-scheduler-pool).
- **The channel is lock-free: one immutable State, CAS transitions**
  (channel-cas, 2026-09-02). channel-callback kept a short
  `synchronized` around the queues; the operator asked for it to go.
  The whole channel is now one immutable value — persistent queues
  for the buffer, the waiting receivers and the waiting senders
  (with their elements), a size counter (immutable.Queue's size is
  O(n)), the open flag, the failure — in an AtomicReference. Every
  operation is a pure `State => (State, action)`; a CAS loop
  installs the new state and only then runs the action (the
  callbacks), so a retry re-runs a pure function and never a
  side effect — the Drive handshake's shape, applied to the whole
  channel. No thread holds anything, ever; on JS the reference is a
  plain cell. Cost: an allocation per operation and a retry under
  contention. Test: eight producers × 1000 through a 16-slot
  channel into four consumers — 8000 elements, each exactly once.
- **A discarded program is a compile error** (discarded-program-lint,
  2026-09-02). The migration to `send: Boolean ! Async` showed the
  hazard of programs-as-values: `c.send(x)` in statement position
  is a value nobody runs, and the first migration pass left ten of
  them (ui, jetty, netty, chatweb) — the -Wall value-discard and
  non-unit-statement warnings saw them, but warnings scroll past.
  build.sbt now escalates exactly those two warnings to ERRORS when
  the discarded TOP-LEVEL type is a `!` program (a regex on the
  message; a `!` nested inside a Queue element type, as in Sim, is
  not matched). Probed shapes: statement `{ c.send(1); () }`, Unit
  def `def f(): Unit = c.send(1)`, eta-expansion `val g: Int => Unit
  = c.send` — all errors; `xs.foreach(c.send)`, `for x <- xs do
  c.send(x)`, `xs.foreach(x => c.send(x))` — invisible to the
  compiler (foreach's `U` accepts anything), stated in AGENTS.md.
  Not testable through munit's compileErrors (lints are reported
  after typer), so the record is this paragraph plus the probe.

## Open boxes
- [x] an error channel in Await — done (see Decisions); callback par
      and joinAsync landed with it
- [x] cancellable Await registrations — done; Timer.after answers the
      canceller on every platform
- [x] the Await-based Channel for JS — SUPERSEDED (channel-callback,
      2026-09-02): that design is now THE channel on every platform,
      src/main/scala/Channel.scala; the parking channel in
      scala-jvm-native is gone (Parallel stays there). Capacity is
      real on JS too: a sender into a full channel suspends as a
      program. Tests: a thousand parked receives hold no thread and
      are freed by offers; a bounded send suspends and resumes on the
      consumer's take; close wakes a parked receiver at once and
      drains a parked sender's accepted element; the 200-round
      send/close race keeps its accounting invariant on the parking
      forms; the cross suite checks refusal after close everywhere.
- [x] run the cross suite on Native — done, in CI
