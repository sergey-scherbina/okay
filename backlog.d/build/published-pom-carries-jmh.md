- [ ] published-pom-carries-jmh — the published `okay_3` pom lists
      `jmh-core`, `jmh-generator-bytecode` and `jmh-generator-reflection`
      1.37 as COMPILE dependencies (no `<scope>`), so every consumer of
      okay's "zero-dependency" core pulls JMH and its jopt-simple,
      commons-math3 and asm transitively. Found 2026-09-23 by the
      scala2-docs lane: `publishLocal` of okay, then
      `~/.ivy2/local/dev.okay/okay_3/0.1.1/poms/okay_3.pom:32-45`, and a
      consumer project's `Runtime/fullClasspath` listing all six jars.
      Likely cause, unverified: sbt-jmh's `JmhPlugin`, enabled on
      okay.jvm (build.sbt:279) and several other published modules
      (418, 442, 495, 1689), adds its jars to the default configuration.
      The fix must be checked the same way: a publishLocal and a pom
      with no jmh in compile scope, and `Jmh/run` still working.
