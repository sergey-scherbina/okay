- [ ] spark-4-2-0-jdk25 — bump okay-spark's Spark dependency from
      4.0.0 to 4.2.0, which resolves the JDK-ceiling problem
      specs/jdk-compatibility.md documented (2026-09-19): Spark 4.0.0
      caps at JDK 21 because Hadoop's UserGroupInformation still
      touches the removed SecurityManager (JEP 486, JDK 24+).

      WHY: checked upstream (Apache JIRA) this session, not assumed --
      SPARK-51167 "Build and Run Spark on Java 25" is Resolved/Fixed
      in 4.2.0 (created 2025-02-11, resolved 2026-05-11 -- before
      today). Spark's own release notes for 4.2.0: runs on Java
      17/21/25 (25 deprecated before 25.0.3; we have 25.0.4.1
      installed this session, which qualifies). If this holds under
      our own build, it removes the ceiling ENTIRELY rather than
      working around it per-feature -- no MRJar needed for Spark, the
      dependency itself would just work across the floor (21) and
      whatever ceiling we want.

      HOW: bump build.sbt:681 ("org.apache.spark" %% "spark-sql" %
      "4.0.0") to 4.2.0. Compile + run okay-spark's existing test
      suite (TestSparkInterop and whatever else lives there) under
      JDK21 first (must still work -- 4.2.0 supports 17/21/25, not
      a JDK25-only jump). If green, ALSO run it under
      ~/.sdkman/candidates/java/25.0.4.1-tem to confirm the ceiling
      really is gone (this is the actual payoff) -- and drop or relax
      TestSparkInterop's `Runtime.version().feature() >= 24` skip
      guard if it now passes.

      RISK: a minor-version Spark bump can carry real API/behavior
      changes (4.0 -> 4.2 spans two minors) -- do not assume
      drop-in; read what breaks if anything does, don't force it to
      compile with casts or suppressions.

      DONE WHEN: okay-spark compiles and its suite passes on JDK21
      (must not regress); if it also passes on JDK25.0.4.1-tem,
      update specs/jdk-compatibility.md's "ceiling" section to say so
      and update/remove the skip guard; gate green (module-scoped is
      fine given compare's unrelated pre-existing break,
      compare/BUGS.md:wroclaw-feed-shadowed).
