- [ ] data-clock-and-random-reach — okay-audit's first real findings
      (2026-10-05, specs/okay-audit.md Results): `okay.Hlc` reads
      `System.currentTimeMillis` and `okay.Uid` reads `scala.util.Random.nextLong`
      directly, so okay-data cannot be a `business` module of the audit and
      a program using an HLC or a Uid is not replayable from its journal.
      The shape: take the clock / the random source as a parameter (a
      `Clock` / `Random` effect, or a plain function) with the platform's
      as the default given, so the pure module is pure and the reach moves
      to the handler. Done-when: `okayData.jvm / auditLayer := "business"`
      in build.sbt and `sbt audit` passes.
