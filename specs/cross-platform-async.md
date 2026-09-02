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
- Channel: the blocking receive is jvm+native; JS gets the
  Await-based channel (callback hand-off) behind the same interface
  where possible.
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

## Open boxes
- [x] an error channel in Await — done (see Decisions); callback par
      and joinAsync landed with it
- [x] cancellable Await registrations — done; Timer.after answers the
      canceller on every platform
- [x] the Await-based Channel for JS — src/main/scala-js/Channel.scala:
      same surface (send/close/receiveAsync + the Stream instance +
      merge/buffer/mergeChunks); capacity is advisory on JS (a sender
      cannot park). The blocking Channel and Parallel moved to
      src/main/scala-jvm-native — one source for both parking
      platforms.
- [x] run the cross suite on Native — done, in CI
