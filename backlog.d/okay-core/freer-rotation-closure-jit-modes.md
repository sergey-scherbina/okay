- [ ] freer-rotation-closure-jit-modes — the machine loops are MULTI-MODAL
      per JVM fork, and the fast mode is the one where the JIT did NOT
      inline `Freer.resume`'s rotation closure. Measured
      (indexed-effects-measure-2, 2026-09-30, `scripts/history.sh
      indexed-effects-measure-2`): `stateLexDeep` on master reads one of
      {91.7, 100.5, 101.5, 103.6, 109.9} us/op per fork, each fork tight
      (±0.3), against a reference (d31fc94f1, the old Delim machine) that
      read 99.7-101.5 over 12 forks; `stateIndexedForward` reads {32.2,
      32.6, 33.1, 33.7, 35.1} against `stateForward`'s stable 30.8-31.7.
      `-XX:+PrintInlining` on four forks of each tree: the ONE fast fork
      (90.9) carries `okay.Freer::resume$$anonfun$1 (20 bytes) failed to
      inline: already compiled into a medium method` at every site where
      every other fork (mine's three slow ones and all four of the
      reference) carries `inline (hot)`. The closure is `resume`'s
      `Bind(Bind(a, f), g) => Bind(a, f(_).flatMap(g))`: inlined into the
      loop, its `f(a)` call site shares the loop's megamorphic profile;
      compiled on its own it keeps its own. Slimming `step` back to the old
      machine's size (1282 -> 672 bytes, done in that lane) removed neither
      the modes nor the fast one. The road to try: the rotation as a NODE
      the loops walk (`Bind(a, Rotated(f, g))`, or the K frame holding the
      pair) instead of a `Function1` the JIT may or may not inline — it
      would also drop the closure allocation per rotation. Price it on
      `stateLexDeep`, `stateIndexedForward` and the Fib/relay lanes
      (`free-tree-is-not-the-cost`: the fused Free loop is the bar).
      RELATED (2026-09-30): freer-kont-frames-probe removes the rotation
      closure altogether (the continuation becomes a queue), so it prices
      this lead as a side effect — measure the modes there before a
      separate lane here.
