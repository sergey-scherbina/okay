- [ ] map-flatmap-pair-cost — PRIORITY: MEDIUM (a hypothesis with a
      number behind it). left-nested-build-cost (2026-09-27) measured that
      a program's global SHAPE is worth ≤1.1x. Yet FusionBenchmark's
      nestedSW (32 µs) and nestedSWr (13 µs) run the same 1000
      State/Writer operations through the same two handlers, and
      BuildShapeBenchmark.rowFoldM, right-nested like nestedSWr, still
      reads 28.8 µs. What differs is each STEP: `sw`/`rowFoldM` perform
      `op.map(acc + _)` and then flatMap, so every operation is
      `Bind(Bind(Inject, mapK), k)`, two binds nested left and rotated
      per step, where `rightSW` does `op.flatMap(k)`, one bind. First:
      one JMH pair that differs ONLY in that (map+flatMap vs a single
      flatMap doing both), to confirm the gap. If it holds, the roads in
      Free: `map` over a `Bind` composing into the existing continuation
      instead of nesting a new Bind, or a map-aware resume case. Either
      one changes `Free.resume`/`map`, and `TestInlineBudget` guards
      resume's size (323 of 325 bytes).
