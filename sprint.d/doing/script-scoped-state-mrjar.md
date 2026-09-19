- [ ] script-scoped-state-mrjar — ship okay.script.api.Scoped as a
      Multi-Release JAR: JDK21-and-up gets the existing ThreadLocal
      backend unchanged, a JVM running on JDK 25+ automatically loads
      a java.lang.ScopedValue backend instead -- one artifact, no
      runtime branching, chosen by the JVM's own classloader per JEP
      238 (Multi-Release JAR Files).

      WHY: follow-up to script-scoped-state (175a2993) -- Scoped[A]'s
      public surface (current/where) was deliberately kept a thin
      facade specifically so the backend could be swapped later
      without touching call sites. Operator asked to do the swap now
      rather than deferring it.

      PROVEN FEASIBLE (this session, scratch/, not committed): a
      ScopedValue-backed Scoped.scala compiles clean against a real
      JDK 25 JVM (Temurin 25.0.4.1, installed this session at
      ~/.sdkman/candidates/java/25.0.4.1-tem) via `java -cp
      <scala3-compiler classpath resolved with cs> dotty.tools.dotc.Main
      -classpath <same> -d out Scoped.scala` -- javac/scalac can only
      see java.lang.ScopedValue when the compiler process ITSELF runs
      on a JDK >= 25 JVM (no -release flag can fake this on an older
      JDK), so this must be a genuinely separate compile, not an sbt
      setting on the main JDK21 session. javap confirmed the public
      method descriptors match the existing base class exactly
      (current(), where(A, Function0), static apply(Function0)) --
      the MR contract.

      HOW: see specs/script-scoped-state-mrjar.md (write first, per
      spec-dev). Shape:
        - okay-script/jdk25/Scoped.scala -- the ScopedValue variant,
          package okay.script.api, OUTSIDE okayScript's own source
          sets so the JDK21 build never tries to compile it.
        - scripts/build-mrjar-jdk25.sh -- locates a JDK 25+ toolchain
          (OKAY_JDK25_HOME env var, else scans
          ~/.sdkman/candidates/java/*), resolves the scala3-compiler
          classpath via `cs fetch --classpath`, compiles the jdk25
          variant with THAT java binary, writes classfiles to
          okay-script/jdk25/target/classes. Never fails the normal
          build if no JDK25 is found -- prints why and exits 0 so it
          is safe to wire into any pipeline unconditionally.
        - build.sbt (okayScript settings): Compile/packageBin/mappings
          picks up okay-script/jdk25/target/classes IF it exists,
          maps each .class under META-INF/versions/25/..., and sets
          the Multi-Release: true manifest attribute only then. A
          machine that never ran the script (any other agent, CI as
          it stands today) gets the exact same jar as before --
          this is additive, not a new hard dependency.
        - both Scoped.scala variants gain a `private[script] def
          backend: String` ("ThreadLocal" / "ScopedValue") purely so
          a test can PROVE which one actually loaded, rather than
          trusting the mechanism by inspection.

      VERIFICATION (cannot go through the normal gate -- the gate
      runs on this box's own JDK 21, so it never exercises the JDK25
      path structurally): package okayScript's jar, run a tiny probe
      class reading Scoped.backend under both
      ~/.sdkman/candidates/java/21.0.12-tem/bin/java and
      ~/.sdkman/candidates/java/25.0.4.1-tem/bin/java against the
      SAME jar, assert "ThreadLocal" and "ScopedValue" respectively.
      Document this gap explicitly in the spec and changelog --
      nothing in CI currently proves the JDK25 path keeps working on
      a future change; a fresh JDK25-side regression would only be
      caught by re-running this probe by hand.

      DONE WHEN: the probe passes both ways on this box; okayScript's
      existing 208 tests still pass unchanged (base path must be
      byte-for-byte the same behavior); scripts/gate.sh "affected
      master" green, cold, on this box's JDK21 (proves nothing broke
      for everyone who does NOT have JDK25 installed).
