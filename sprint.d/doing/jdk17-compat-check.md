- [ ] jdk17-compat-check — an opt-in way to answer "does this run on
      JDK 17 today" per module, push-button, so the answer is a
      command away rather than re-derived by hand each time (the way
      this session did it three times already, for Scoped, Schedulers,
      and now this).

      WHY: operator asked "how do we optionally build for JVM 17".
      Compiling FOR 17 needs no flag -- dotc hosted on 21+ already
      emits JDK17-loadable bytecode (major version 61) by default,
      proven repeatedly this session. The real question is RUNTIME
      safety: which modules still call a JDK21+ API unconditionally.
      specs/jdk-compatibility.md's table lists 6 by inspection
      (okay-http, okay-jetty's own direct call, okay-netty,
      okay-cluster, okay-persist, okay-script) -- this task turns that
      into something RUN, not just read, and gets real current data
      instead of a table that can go stale the way the "breaks at
      compile time" line already did once.

      HOW: `integrationTest`-shaped sbt command alias
      (`verifyJdk17` or similar) -- `set every Test / javaHome :=
      Some(file(<jdk17 home>)); test` -- pointed at the JDK17
      candidate this session already installed
      (~/.sdkman/candidates/java/17.0.19-tem). Only takes effect
      where Test/fork is already true, same mechanism
      jdk26-default-runtime already uses; additive, no hard new
      dependency for a machine without the candidate (same shape as
      build-mrjar-jdk25.sh / the JDK26 default).

      RUN IT for real data, not assumed: expect the known 6 modules
      to fail and expect okaySpark/okayDelta to PASS this time (their
      ceiling is JDK 24+ specifically -- JEP 486 removing the Security
      Manager -- and 17 is well under that, unlike 26; if they fail
      too that is new information, not confirmation of anything
      already believed).

      DONE WHEN: the command exists and its first real run's findings
      are recorded in specs/jdk-compatibility.md, replacing the
      by-inspection table with by-measurement data (which modules
      actually pass, which fail and with what exception) -- this
      task does NOT fix the 6 modules, only makes their status
      checkable and current.
