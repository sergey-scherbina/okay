- [ ] mrjar-jdk25-ci-gap — CLOSED BY BUILDING THE VARIANT IN sbt
      (operator, 2026-09-25: "насчёт mrjar — это нужно исправить";
      promoted from backlog.d/build the same day). The gap: the JDK-25
      variant `jdk25/Scoped.scala` is compiled only by
      `scripts/build-mrjar-jdk25.sh`, run by hand, so a published jar
      has it or not depending on who built it, and nothing in the gate
      loads it — worse, since scoped-cross-platform moved `Scoped` to
      okay-platform the packaging stayed on the CORE's jvmSettings
      (build.sbt ~344), so the variant lands in a jar whose root has no
      `Scoped` at all. The reason for the script is gone: sbt compiles
      on JDK 25 since java-gatherers. PLAN: (1) `jdk25/` becomes an sbt
      project compiled on every build (`-java-output-version 25`, no
      project dependsOn — okay-platform's classes dir on its
      classpath), through a `versioned(<project>, n)` helper so
      cont-stack can add `jdk22/` (the FFM StackRoom reader,
      specs/cont-stack.md Decision 12) to the CORE the same way;
      (2) its classes are packaged under `META-INF/versions/25/` of
      okay-platform's jar with `Multi-Release: true`, ALWAYS; (3) the
      forked JVM tests of a project with versioned classes run against
      its PACKAGED jar (jar first on `Test / fullClasspath`, classes
      dir dropped), so `TestScopedBackend` asserts "ScopedValue" on
      25+ and "ThreadLocal" under `verifyJdk17` — the probe the gap
      item asked for, in the gate; (4) the script and the core's
      packaging block are deleted, script-scoped-state-mrjar.md gets
      the decision, jdk-compatibility.md "Compiling on 25" is updated.
      Verify: the jar listing shows the versioned entries and the
      manifest attribute; the two backend assertions; `verifyJdk17`
      on okay-platform.
