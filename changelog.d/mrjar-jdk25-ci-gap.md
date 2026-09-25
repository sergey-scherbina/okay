## mrjar-jdk25-ci-gap — the Multi-Release variant is built by the build, in every jar, tested through it

The operator, told that a published jar carried `Scoped`'s JDK 25
variant only when whoever built it had run
`scripts/build-mrjar-jdk25.sh` by hand: "это нужно исправить"
(2026-09-25). The script's reason — dotc on a JVM without
`java.lang.ScopedValue` — has been gone since java-gatherers moved sbt
to 25.

- build.sbt `versioned(id, dir, n, host)`: a Multi-Release variant as
  an sbt project, `-java-output-version n`, compiled against the
  host's compile classpath (the root class is on it; the source wins,
  measured). `multiRelease(variant, n)` on the host: every class into
  `META-INF/versions/n/`, `Multi-Release: true`, always; and the
  host's forked tests run against the packaged JAR, first on the test
  classpath in place of both classes directories, because a class
  loaded from a directory is never versioned.
- `jdk25/Scoped.scala` is project `okayPlatformJdk25`; okay-platform
  depends on it `test->compile` and packages it. The core's copy of
  the packaging — still on `okay` after the class moved to
  okay-platform, so packaging a variant into a jar whose root had no
  `Scoped` — is deleted, with the script.
- okay-platform's tests fork now (`Test / javaHome`: 26 by default,
  17 under `verifyJdk17`). `TestScopedBackend` asserts `Scoped` came
  from a jar and that the backend is the one the running JDK must
  have picked, and prints the JDK. Measured through the gate:
  `JDK 26.0.2.1 loaded the ScopedValue backend`, `JDK 17.0.19+10
  loaded the ThreadLocal backend`.
- specs/script-scoped-state-mrjar.md closes with the decision;
  jdk-compatibility.md and java-gatherers.md updated where they
  named the script. specs/cont-stack.md's `jdk22/` reader (the FFM
  stack pointer) is the next user of `versioned`.
