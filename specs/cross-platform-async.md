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
- [ ] one shared-source test suite passes on all three platforms
      (Await-based programs only)
- [ ] a JVM/Native-only test exercises blocking Run/join under the
      capability
- [ ] on JS, a sleep-then-answer program completes via runAsync
      without blocking the event loop
- [ ] cancellation works on all platforms (Await registrations are
      cancellable)

## Decisions
- **Await(register) over a Promise/Future-shaped op** — no dependency,
  no platform type in the core signature; adapters live at the edges.
- **Capabilities over subsetting**: JS does not get a crippled Fiber
  type; it gets the same type minus the evidence-gated methods.
