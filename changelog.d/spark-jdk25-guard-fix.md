## spark-jdk25-guard-fix — okay-spark's JDK25 support confirmed directly, guard corrected
Landed: 2026-09-19

Follow-up to spark-4-2-0-jdk25: its "local JDK25 run inconclusive,
sbt/JDK tooling wrinkle" finding was wrong. `TestSparkInterop.
munitIgnore` (`javaFeature >= 24`, written for Spark 4.0.0) was
silently skipping every test on JDK 25 too — a skip is exactly what
sbt's fork reports as "0 tests, no exception". Narrowed the guard to
`== 24` (the one version with no workaround) and verified 25 for real
with a standalone `java` process bypassing sbt's fork: a genuine
distributed Spark job passed (`local=8333333.25 onSpark=8333333.25
diff=0.0`) under `~/.sdkman/candidates/java/25.0.4.1-tem`.

Spark 4.2.0 on JDK 25 is now directly confirmed in this repo, not
just upstream-claimed. See specs/jdk-compatibility.md.
