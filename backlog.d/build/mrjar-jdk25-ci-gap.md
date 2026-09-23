- [ ] mrjar-jdk25-ci-gap — (moved from okay-script to build by
      backlog-audit-0923: `Scoped` lives in okay-platform since
      scoped-to-core — `okay-platform/src/main/scala/Scoped.scala`, the
      JDK-25 variant in the root `jdk25/Scoped.scala`, packaged into
      `META-INF/versions/25/` only when `scripts/build-mrjar-jdk25.sh`
      has run.) Nothing automatically re-verifies the JDK25
      (ScopedValue) side of that Multi-Release JAR
      (script-scoped-state-mrjar, 2026-09-19).
      scripts/gate.sh runs on whatever JDK is on the box (JDK 21 on
      the machines seen so far), which structurally cannot load the
      META-INF/versions/25/ variant -- a future change to
      jdk25/Scoped.scala that broke it would go unnoticed
      until someone ran the manual probe in specs/
      script-scoped-state-mrjar.md by hand.

      Two ways to close it, neither attempted here: (a) get a JDK 25+
      runner into the gate/CI pipeline and add an automated version of
      the probe (package the jar, run a tiny class reading
      Scoped.backend under that JDK, assert "ScopedValue"); (b) decide
      the project's baseline JDK is moving to 25 everywhere, which
      would make the whole Multi-Release split unnecessary and this
      item moot. Not urgent: the base (JDK21) path is unaffected by
      drift on the JDK25 side, so a broken variant fails silently
      rather than breaking anyone's build -- it just quietly stops
      giving JDK25 users the ScopedValue benefit.

      UPDATE 2026-09-23 (java-gatherers): half of (a) is done by
      accident. sbt compiles on JDK 25 now and every forked Test runs
      on 26, so the gate HAS a 25+ runner; what is still missing is
      the probe itself (and `jdk25/Scoped.scala` could
      become an ordinary source compiled by sbt with `jdkFloor(0)`,
      guarded like okay-platform's Loom, instead of an out-of-sbt
      script — see specs/jdk-compatibility.md "Compiling on 25").
