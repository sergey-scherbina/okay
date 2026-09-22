## java-gatherers - a Stage IS a JDK Gatherer; sbt compiles on JDK 25

okay-java's `Gather` bridges JDK 24's stream gatherers (JEP 485) both
ways: `Gather.gatherer(stage)` runs any okay `Stage` inside
`stream.gather(…)` — a stage that answers short-circuits even an
infinite stream, a refused push stops it mid-element — and
`Gather.stage(g)` runs any JDK gatherer (`windowFixed`, `scan`, …) in an
okay pipeline. `Windowed.gatherer` is the event-time window as a
gatherer: each pane pushed the moment the watermark closes it, and the
parallel stream the collector must refuse simply gives the right panes.
Law-tested both directions; four mutants each fail their own test.

The build: `Gatherer` exists in no JDK 21 class library, so sbt runs on
JDK 25 now (`.sdkmanrc`, operator's choice) and the floor that compiling
on 21 used to imply is a flag — `-java-output-version 17` on every
Scala 3 module, `jdkFloor(n)` where a module differs (the compiler named
three: `okay-platform`/`okay-http` 0, `compare` 21; okay-java 0), test
code unflagged (project/JdkFloor.scala). Bytecode unchanged, 61
everywhere but `compare`. `okay-delta`'s tests, pinned to the ambient
JDK for Hadoop's sake, failed on 25 at once and are pinned to a named
21 now.

Found: a pipeline built by `through` over a stateful stage is one-shot
— `Gather.stage` refuses a re-run by name; okay-stream's `Windows.stage`
silently drops a pane on one (backlog windows-stage-rerun-loses-pane).
And the gate's stall watchdog misses the idle-runner hang it was built
for (backlog gate-watchdog-idle-sbt-cpu).

Docs: guide §5, theory ch. 7 (gatherers as the push-form enumeratee,
Hickey's transducers), docs/modules/okay-java.md, specs/jdk-compatibility.md,
AGENTS.md's JDK bullet. specs/java-gatherers.md.
