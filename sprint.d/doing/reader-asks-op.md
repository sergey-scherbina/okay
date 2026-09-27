- [ ] reader-asks-op — the Reader half of op-map-constructors (2026-09-27,
      State half landed: `State.Update`, stateUpdate 47.4 → 21.0 µs).
      `Reader.read` and `Reader.lift` are `ask.map(f)`, a shared `Ask`
      node plus a map. Under a caller's flatMap that is two binds nested
      left every step. There are 17 uses in main. A `Reader.Asks(f: R => A)`
      operation answers `f(r)` in one. The ripple is wider than State's:
      `Ask()` is matched in Reader.scala, Staged.scala (stagers) and
      Bisim.scala, in okay-kyo (KyoInterop), in okay-direct's tests and
      StagedBenchmark, and in about 12 core tests (grep `Reader.Ask()` and
      `case Ask()`). `Direct.staged` reads the shared `askNode` through
      DirectRow's shared-node table, and an `Asks` must stage too, so a
      TestStagers case is needed. Measure first: a BuildShapeBenchmark
      lane of 1000 foldM steps of `Reader.read`, before and after, as
      stateUpdate was.
