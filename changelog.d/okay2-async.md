## okay2-async and okay2-platform - the Async effect and the JVM under it, for the Scala 2 core

Two subprojects in the `okay2/` build. `okay2-async` (package
`okay2.async`) is the effect: `Async` with `Run` and `Await`, the
callback `Drive`, `Fiber`/`Scheduler`/`Timer`/`CanBlock`/`Handoff` as
traits, `Async(a)` and `await`, `run`/`runAsync`/`spawn`/`par`/`race`/
`timeout`/`attempt`/`sleep`/`supervised` (a nursery as a parameter),
`Retry`, `Par`. `okay2-platform` (package `okay2.platform`) is the JVM:
`CanBlock` parking a virtual thread with the interrupt read first, the
scheduled-executor timer, the scheduler family (`loom`, `forkJoin`,
`drive`, `own`/`adaptive` with the Chase-Lev deque, `threads`, `auto`),
`Threads`, `Interruptible`, `Scoped`, `Net`, `parAll`/`parTraverse`/
`retry`/`supervised`. 34 tests mirror TestAsync, TestAsyncCross,
TestSupervised, TestPar, TestParallel and TestScoped; 166 in the okay2
gate.

- `Async(a)` is the suspend constructor: a function `async` in the
  package object was ambiguous with the package of the same name.
- Platform defaults reach programs through IMPLICIT SCOPE (one
  `PlatformDefaults` implicit, derived per capability in the
  companions), so a local `implicit val S: Scheduler` overrides
  without the ambiguity three package-level implicit vals produced.
- `okay2/.sdkmanrc` pins the gate's JDK to the root's 25: scripts/
  gate.sh reads the .sdkmanrc of the directory it runs in, and
  okay2-platform names virtual threads (compiles on 21+, runs on 17+).
- Not ported: `Blocking[A]` (a context function), `Failing`
  (Resource), `SharedOnce` (Once), `Operations`, JS/Native, the
  scheduler soaks (backlog `okay2-scheduler-laws`).

Docs: docs/okay2.md section 11; specs/okay2.md stage 4.
