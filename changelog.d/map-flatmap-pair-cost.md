## map-flatmap-pair-cost - `p.map(f).flatMap(g)` builds one Bind: 1.17-1.23x on map-heavy programs

- `map` used to be `flatMap(a => Return(f(a)))`, so `op.map(f).flatMap(g)`,
  the commonest step there is, built two binds nested left. `resume`
  rotated them on every step. Measured first: 28.6 µs / 306 KB against
  12.6 µs / 138 KB when each step is one flatMap (BuildShapeBenchmark
  rowFoldM / rowOneBind, the new lane).
- `map` now leaves a `Free.Mapped` continuation, and a flatMap or map on
  top of it composes into ONE Bind. Composition is bounded at 32 maps
  (stack safety), and `resume` is untouched.
- A/B against master: FusionBenchmark.nestedSW 32.1 → 26.2 µs (1.23x,
  −17% B/op), rowFoldM 1.17x (−18%), stateFoldM 1.21x (−29%). The
  map-free lanes (relayPrebuilt, handlePrebuilt, fusedSWr) are unchanged,
  with identical bytes.
- TestMapFusion (5, watched red). specs/map-fusion.md. The rest of the
  gap is filed as `map-fusion-residual`.
