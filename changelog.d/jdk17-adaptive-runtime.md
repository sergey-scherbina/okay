## jdk17-adaptive-runtime — four of six modules genuinely fixed for JDK 17; two surfaced a deeper problem

Implemented the plan from specs/jdk17-adaptive-runtime.md: a shared
`okay.Threads.spawn`/`spawnThread` helper in core (adaptive, same
`Schedulers.hasVirtualThreads` check `Schedulers`/`Timer` already use),
applied at all seven unconditional JDK21+ call sites across the six
modules `jdk17-compat-check` had measured as broken, plus the same
treatment for the test-harness code that also called `Thread.ofVirtual`/
`startVirtualThread` directly (found along the way, not in the original
plan).

Measured on real JDK 17 (`~/.sdkman/candidates/java/17.0.19-tem`, `Test/fork
:= true`, `--include-tags=Live` where the real suite carries that tag —
2026-09-20):

- **`okay-script`** 208/208, **`okay-cluster`** 136/136, **`okay-persist`**
  242/242 default + 13/13 `Live`, **`okay-netty`** 16/16 `Live` — all
  genuinely green, no caveats.
- **`okay-jetty`**: the `VirtualThreadPool` crash is fixed (conditional
  `QueuedThreadPool` fallback); a second, unrelated bug was found and
  fixed along the way — `Listen.java` (the module's one Java source) was
  compiled to classfile 65 (JDK 21) by `javac`'s host-JDK default, unlike
  `dotc` which always targets 61 — `javacOptions ++= Seq("--release",
  "17")` fixes it. But `TestResumable` (the SSE resumable stream) still
  hangs on real JDK 17.
- **`okay-http`**: the `Executors.newVirtualThreadPerTaskExecutor()` crash
  is fixed (`newCachedThreadPool()` fallback); but `TestNio` (raw blocking
  NIO, scheduled through `Async.spawn`) genuinely deadlocks — traced to
  `Schedulers.own`'s bounded platform-thread pool starving under
  concurrently-blocked accept/read fibers, the same mechanism
  jdk17-compat-check had already found causing wrong-timing failures
  (not hangs) in `okayJVM`'s own `TestPar`/`TestDirectParallel`.

Not a regression on the shipping path: the project's real default JDK
(26) has virtual threads, so none of this triggers there — full `sbt
integrationTest` on the ambient JDK stayed green throughout, `okayJetty`
included.

The remaining hang is recorded as its own backlog item,
`backlog.d/okay-http/schedulers-own-hangs-under-blocking-nio.md`, with
three real fix options named and none chosen — separate, harder work,
not attempted here.
