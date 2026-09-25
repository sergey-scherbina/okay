- [ ] stack-safety-catch-up-okay2 — stack recursions that landed in okay2
      AFTER the 2026-09-25 inventory, named by the stage-9 guard's first
      run (stack-safety-guard). okay2-sql's `Query.render`/`both`/
      `collect`/`eval` are CLOSED (stack-safety-query: a predicate folded
      200 000 deep overflowed; explicit stacks now, in both cores). LEFT,
      as UNAUDITED rows in specs/stack-safety-okay2.tsv: okay2-sql's
      `Typed.fits`, okay2-jdbc's `valueOf`/`arrayOf`/`jdbcOf`, okay2-spark's
      `SparkSchema` walks (`dataType`, `struct`, `value`, `rowOf`), and the
      fs2/zio interop `again`s. The jdbc and spark ones recurse per level of
      a schema or a nested value; the interop ones through the library's
      own bind, which may already be deferred (check how fs2/ZIO run it
      before writing a stack). Red first on a small stack, then an explicit
      stack or a written bound replacing UNAUDITED. (2026-09-25)
