## op-map-constructors - `State.update`/`swap` are one operation: 2.26x, −55% memory

- `State.Update(f: S => (B, S))` is new. `update` and `swap` used to be a
  get, then a set, then a map to answer: three binds, two of them nested
  left under the caller's flatMap. They are one operation now. So are
  `zoomWith`'s part operations, and an `Update` on the part is one
  `Update` on the whole.
- Every State matcher learns the case: the handler, zoom,
  `Bisim.Answers.state`, the Lexical clauses, the three stagers, the
  test handlers, and DelimBenchmark/SplitBenchmark.
- BuildShapeBenchmark.stateUpdate (the new lane): 47.4 → 21.0 µs, 463 →
  207 KB per 1000 steps. That is `modify`'s level.
- The same move for Reader (`read`/`lift` are `ask.map(f)`, 17 uses in
  main) is queued as `reader-asks-op`. specs/effect-row-cost.md D4.
