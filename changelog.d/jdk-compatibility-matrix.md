## jdk-compatibility-matrix - the floor, the ceiling, written down

No code changed. `specs/jdk-compatibility.md` names the two
constraints that pull the project's JDK range in opposite directions,
neither written down anywhere before: the floor is JDK 21 —
`Platform.scala`'s DEFAULT `Async` scheduler is Loom (virtual
threads, JEP 444, non-preview since 21), which is the JVM runtime's
own execution model rather than a few modules' opt-in, and seven
JVM-only files reach `Thread.ofVirtual`/`startVirtualThread`/
`newVirtualThreadPerTaskExecutor` directly (core `Platform`,
okay-http's JDK backend, okay-jetty, okay-netty, okay-cluster,
okay-script, okay-persist). The ceiling is okay-spark on JDK 24+:
Hadoop's `UserGroupInformation` reaches the Security Manager, removed
outright by JEP 486, no flag reaches past it — `TestSparkInterop`
already skips itself there rather than fail opaquely.

`.sdkmanrc`'s pinned `21.0.7-tem` is exactly the floor, inside both
constraints. JDK 25 gets its own paragraph: not a floor-or-ceiling
question, an *additive* per-feature answer (`okay.Scoped`'s
Multi-Release JAR, `specs/script-scoped-state-mrjar.md`), with the
CI gap that leaves unverified named in `backlog.d/okay-script/
mrjar-jdk25-ci-gap.md` — left open, not this item's to close.

A pointer lands in `AGENTS.md`'s "Build facts that bite" so the next
"can we bump the JDK" conversation starts from facts.
