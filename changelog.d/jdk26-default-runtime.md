## jdk26-default-runtime — Test/run default to JDK 26, compile stays JDK 21
Landed: 2026-09-19

Two separate knobs: the ambient JVM that launches sbt and compiles
everything stays JDK 21 (`.sdkmanrc` unchanged). A forked `Test`/`run`
now defaults to JDK 26 (the newest GA release; 27 is not GA at this
date) — additive, only where `Test/fork`/`run/fork` was already
`true`, no hard new dependency for a machine without the candidate.

`okaySpark` (already pinned to 25, spark-jdk25-guard-fix) and
`okayDelta` (newly pinned to 21 — Delta Kernel hits the same Hadoop/
`UserGroupInformation`/JEP486 wall Spark does, found by this lane's
first full-matrix run on 26, no upstream fix available yet) shadow
the build-wide default back down.

Verified directly: okayJVM 746/746, okayScript 208/208 (its own
in-process dotc now hosted on 26), okayJetty's real integration suite
19/19 (actual sockets, WebSocket, `VirtualThreadPool` under 26),
okaySpark 4/4, okayDelta 4/4. Gate: 181 modules, 5664 tests, no
warnings, GREEN.

See specs/jdk-compatibility.md.
