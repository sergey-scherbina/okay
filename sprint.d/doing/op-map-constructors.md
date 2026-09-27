- [ ] op-map-constructors — ORDER 1 of the map-cost plan (operator,
      2026-09-27). A step made of an operation and a `map`, followed by
      the caller's flatMap, is two binds nested left. `resume` rotates it
      every step, and it costs ~7 objects against 2
      (specs/map-fusion.md: 28.6 vs 12.6 µs, 306 vs 138 KB per 1000
      steps). The library's own CONSTRUCTORS that answer `op.map(...)`
      hand every caller that shape: `State.update` and `State.swap`
      (`get` then `set(next).map(_ => b)`), and whatever a grep of
      `.map(` in constructor bodies finds (Writer, Reader, Throws,
      Supply, Once, Chronicle...). The fix is the one effect-row-cost D1
      proved on `State.modify` (384 → 168 B a level): a ONE-operation
      form (`State.Update(f: S => (B, S))`, answered by the handler), with
      every exhaustive match on the signature learning the case, the
      Jmh sources included (`Test/compile` does not reach them; that is
      how six E029 warnings sat after D1). Measure each converted
      constructor in a BuildShapeBenchmark-style lane, with the build
      inside the method, before converting the next.
