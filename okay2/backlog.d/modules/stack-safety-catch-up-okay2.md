- [ ] stack-safety-catch-up-okay2 — fourteen stack recursions landed in
      okay2 AFTER the 2026-09-25 inventory and were named by the stage-9
      guard's first run (stack-safety-guard): okay2-sql's `Query.render`/
      `both`/`collect`/`eval` and `Typed.fits`, okay2-jdbc's `valueOf`/
      `arrayOf`/`jdbcOf`, okay2-spark's `SparkSchema` walks, and the fs2/zio
      interop `again`s. They are rows marked UNAUDITED in
      specs/stack-safety-okay2.tsv, so the guard holds. `Query.eval`/
      `collect` recurse per `And`/`Or` of a predicate built by a FOLD —
      the same suspect stage 4 names for the Scala 3 okay-sql; the jdbc
      and spark ones recurse per level of a schema or a nested value.
      Take each red first on a small stack, then an explicit stack or a
      written bound replacing UNAUDITED. (2026-09-25)
