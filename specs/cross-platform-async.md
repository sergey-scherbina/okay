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
      (Await-based programs only) — src/test/scala-cross runs on JVM
      and JS (under Node); Native compiles the same code but its test
      run is still stubbed in the build (nativeLink cost), so the
      third platform is compile-verified only
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
- **par stays under CanBlock** — a child failure must propagate, and
  the Await callback carries no error channel; race is cross-platform
  because a failing contender never wins by design.

## Open boxes
- [ ] an error channel in Await (register: (Either[Throwable,A] =>
      Unit) => Unit or a richer op) — unlocks callback par and
      Fiber.joinAsync without the hang-on-failure trap
- [ ] cancellable Await registrations (register answers a canceller;
      Timer.after already could)
- [ ] the Await-based Channel for JS behind the same interface
- [ ] run the cross suite on Native (unstub nativeLink in CI)
