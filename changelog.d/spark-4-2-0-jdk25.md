## spark-4-2-0-jdk25 — Spark bumped to 4.2.0, JDK ceiling raised on the 21 end
Landed: 2026-09-19

Spark 4.0.0 -> 4.2.0: upstream fixed JDK25 support (SPARK-51167,
resolved 2026-05-11). Compiles as a drop-in; runtime needed the
existing `legacyStdlib` fix's paired version bumped alongside
`scala-reflect` (both to 2.13.18) — a "mixed pair" mistake, not a new
problem, and corrected a stale build.sbt comment that had drawn the
wrong conclusion from the same mistake.

Verified: all 4 TestSparkInterop tests pass on this box's JDK 21
floor. NOT verified: an actual local JDK25 run (sbt's test-fork
discovered zero tests under a JDK25 `Test/javaHome` — an sbt/JDK25
tooling wrinkle here, not a Spark defect per Spark's own JDK25 CI).
`TestSparkInterop`'s `munitIgnore` guard (`>= 24`) stays in place
until that's actually confirmed locally.

See specs/jdk-compatibility.md.
