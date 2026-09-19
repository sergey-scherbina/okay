- [ ] jdk26-default-runtime — JDK 26 (Adoptium GA, most recent
      feature release as of 2026-09-19; 27 is not GA yet) becomes the
      default JVM for TEST/RUN across the project, except okaySpark
      which stays pinned to a confirmed-safe version. Compilation
      (sbt's own JVM, in-process dotc) is UNCHANGED, still JDK21 --
      that is what .sdkmanrc pins and nothing here touches it.

      WHY: operator asked for JDK17-compile/JDK27(or 26)-runtime as a
      default split. Checked before promising anything: dotc hosted
      on JDK21+ already emits JDK17-loadable bytecode by default
      (jdk-adaptive-scheduler's own probe proved this), so "compile
      FOR 17" is already true; "compile WITH a JDK17-hosted compiler"
      is a different, much bigger ask -- 6 modules (okay-http,
      okay-jetty's own direct call, okay-netty, okay-cluster,
      okay-persist, okay-script) reference JDK21+ APIs unconditionally
      in source, and each would need the SAME per-callsite adaptive
      treatment jdk-adaptive-scheduler gave Schedulers before a
      JDK17-hosted compile could even resolve their symbols. Operator
      picked the smaller, safe slice: runtime only, JDK26, Spark
      excluded (checked: Spark 4.2.0's own confirmed range is
      17/21/25 -- no JDK26 JIRA found; .sdkmanrc's own comment already
      says JDK26 refuses the security-manager flag outright).

      HOW: `ThisBuild / Test / javaHome` and `ThisBuild / run /
      javaHome` default to the installed JDK 26
      (~/.sdkman/candidates/java/26.0.2.1-tem) -- only takes effect
      where `Test / fork` / `run / fork` is already true, which is
      the existing convention across most test-bearing modules
      (checked, not assumed). `okaySpark` gets its own `Test /
      javaHome` / `run / javaHome` override back to the confirmed-safe
      JDK. `.sdkmanrc` itself is untouched (still pins 21 for the
      ambient/compile JVM) but its comment gets a pointer to this.

      VERIFICATION: this is the first time the WHOLE test matrix runs
      on JDK26 -- genuinely unknown territory beyond Spark's own
      already-checked ceiling. Expect this to surface NEW
      incompatibilities in the dependency tree (kyo/zio/cats-effect/ox/
      jetty/netty/etc, none checked against 26 before). Triage each
      failure the same way this session already did twice today
      (compare's wildcard-import bug, the board-pointer merge): control
      experiment against the un-JDK26'd tree to tell "caused by this"
      from "pre-existing, just never run under 26 before" -- do not
      assume either direction. Do not chase every finding to a full
      fix in one sitting if the surface is large; record what doesn't
      fit this session's remaining budget as its own BUGS.md/backlog
      entry rather than leaving it undocumented.

      DONE WHEN: a representative sample (okayJVM, okayScript,
      okayJetty, okayHttp) passes its own test suite under JDK26
      directly (not just "gate didn't fail on something forked
      elsewhere"); okaySpark's suite still passes on its pinned
      version; full gate green OR every red finding triaged and
      recorded (fixed, or filed as its own tracked item) rather than
      silently ignored.
