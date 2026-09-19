## jdk-adaptive-scheduler — the default Scheduler/Timer adapt to whether the JVM has virtual threads
Landed: 2026-09-19

`given Timer` and the default `given Scheduler` called
`Thread.startVirtualThread` unconditionally — a NoSuchMethodError the
first time anything fired or forked a fiber on a JDK without virtual
threads, no fallback. An earlier plan (splitting `Schedulers` across
JDK versions the way `okay.Scoped` does, Multi-Release JAR) was wrong
and refuted directly by experiment before being written: JVM
constant-pool resolution is lazy per call site, not per class — a
class file referencing `Thread.startVirtualThread` in one method
already loads and runs fine on JDK 17, calling any other method in it.

`Schedulers.hasVirtualThreads` (checked once) and `Schedulers.auto`
(named, public, callable directly from code) pick `loom` where
available, `own` (the fastest platform-thread scheduler measured
here) where not. `given Timer` and `given Scheduler` both use it;
`-Dokay.scheduler` still overrides, and an explicit `loom` request
degrades to `auto` rather than crashing when virtual threads aren't
actually there.

Verified directly on both ends: JDK 21 — 743 tests unchanged. JDK 17
— a standalone probe (bypassing sbt's own always-JDK21+ test loop)
against the packaged jar: `hasVirtualThreads: false`, `auto` picks
`own`, a timer fires correctly, `-Dokay.scheduler=loom` correctly
degrades to the `Owned` implementation class.

Makes `okayJVM` core genuinely correct on JDK 17 — does not move the
project's floor (okay-http/jetty/netty/cluster/persist/script still
call JDK21-only APIs directly). See specs/jdk-adaptive-scheduler.md,
specs/jdk-compatibility.md.
