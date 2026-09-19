- [ ] spark-jdk25-guard-fix — TestSparkInterop's `munitIgnore` was
      `javaFeature >= 24`, written for Spark 4.0.0's ceiling. After
      spark-4-2-0-jdk25 bumped Spark, this guard was never updated --
      it kept skipping ALL tests on JDK 25 too, and that skip is what
      the previous session misread as "sbt's test-fork discovers zero
      tests under JDK25, looks like a tooling wrinkle." It was not a
      tooling wrinkle. `sbt.ForkMain` debug output showed the suite
      WAS run and DID pass as a task (0 tests inside is exactly what
      `munitIgnore` produces) -- the fix is `javaFeature == 24`, not
      chasing an sbt/JDK bug that was never there.

      HOW: narrow the guard to exactly 24 (the one JDK version with NO
      workaround -- JEP 486 removed SecurityManager outright; 22/23
      still have it, deprecated but present, `-Djava.security.manager
      =allow`, and are simply UNTESTED here, not assumed broken).
      Then actually verify 25 for real: a standalone `java` process
      (not sbt's fork) running TestSparkInterop's own
      SparkSession-creation-and-aggregation path under
      ~/.sdkman/candidates/java/25.0.4.1-tem, same technique as the
      Scoped MRJar probe. Update specs/jdk-compatibility.md and the
      spark-4-2-0-jdk25 changelog entry with the corrected finding.

      DONE WHEN: the standalone JDK25 probe passes with real assertion
      output (not just process-exit-0); okaySpark/test still passes on
      JDK21 (guard narrowing must not change the 21 path); gate green.
