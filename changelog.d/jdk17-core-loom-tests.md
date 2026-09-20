## jdk17-core-loom-tests - the core suite is green on a real JDK 17

`okayJVM/test` on JDK 17.0.19: 754 passed, 0 failed, 8 skipped (was
736/746). Three suites had died on the first `NoSuchMethodError` —
munit treats a linkage error as fatal and skips the rest of the
suite, so one test naming `Schedulers.loom` took fifty-nine others
with it. The split is by what the test is ABOUT: the `loom` member
of `SchedulerFamily` and "spawn runs on a virtual thread" `assume`
virtual threads and skip without them; the tests that only used a
JDK 21 `Thread` API to carry fibers — `TestDirectParallel`'s
counting scheduler, `TestTDict`'s racers, the deque law's thieves —
carry them on `Schedulers.threads`, `okay.Threads.spawnThread` and
`new Thread`, and are the same tests on either JDK.
`TestAdaptiveScheduler` no longer asserts that the JVM it is on has
virtual threads: it checks the flag against `Thread.ofVirtual`
itself and asserts the branch that JVM takes, so `sbt verifyJdk17`
tests the other branch for real. specs/jdk-compatibility.md's
`okayJVM` row and its `TestPar`/`TestDirectParallel` caveat are
updated: both pass on 17 since own-lost-wakeup.
