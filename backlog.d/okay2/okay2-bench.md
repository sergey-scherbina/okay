- [ ] okay2-bench — PRIORITY: HIGH, first of the okay2 items. The Scala 2
      core has no `inline` (200 `inline def`s in the Scala 3 core are
      ordinary methods here) and its handler loops answer through an
      `Either` per operation where the Scala 3 core's inline `split`/
      `onAnswer` branches are the caller's own tail call. NOTHING IS
      MEASURED. The lane: one JMH lane in `okay2/src/jmh` mirroring an
      existing Scala 3 lane exactly (sbt-jmh in okay2/build.sbt, its own
      build; HandlerBenchmark's State lane, or
      relayForward), same program shape, same size, run per the
      per-lane gated protocol (docs/benchmarks.md), recorded in
      src/jmh/history.tsv with the Scala 3 number beside it. What
      would settle it: a ratio; under 1.3x means "no inline" costs
      what the JIT gives back, over it names the first thing to fix
      (the Either per op is the first suspect — a sentinel instead of
      Left/Right removes the allocation). (2026-09-24)
      ADDED 2026-09-24 (review): the Either is not the only per-operation
      allocation. `State.handleAt` allocates, per handled operation, the
      two closures passed to `split` (they capture `s` and `k`), a
      `Tuple2`, and the `Left` — four objects where the Scala 3 core's
      inline split has none of them. A non-allocating form keeps the
      casts in `Split`: `Split.isF[F](e)` plus `Split.asF`/`asG`, and the
      loop branches with `if` — measure both before choosing.
