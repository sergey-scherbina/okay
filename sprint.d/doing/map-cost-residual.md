- [ ] map-cost-residual — WHERE THE OTHER FOUR FIFTHS OF THE map+flatMap
      GAP ARE (operator ask, 2026-09-27). specs/map-fusion.md reads the
      2.27x of `op.map(f)`-then-bind against one flatMap (28.6 vs 12.6 µs)
      as "the step's two binds". The repository's own numbers say the two
      binds are about a FIFTH of it: with the map fused in the DIRECT form
      — `foldM`'s `step`, `Bind(op, y => next(f(y)))`, one-bind-hot-steps
      — rowFoldM still reads 23.4 against rowOneBind's 11.3-12.6 (history
      `…-bind-continuation-queue.tsv`, master arm), and nestedSW under the
      refuted direct fusion read 26.2 against nestedSWr's 13.7. Both
      refuted lanes (map-fusion, bind-continuation-queue) attacked the
      fifth. Before any further Free change: NAME the residual. Candidates,
      from `Effects.scala:411-442` and 231 vs 127 KB/op (+104 B a step):
      the `Bind`+`Mapped` that `op.map` builds and `step` discards (40 B);
      two closures a step (`go`'s and `step`'s) where rowOneBind has one;
      boxing of the erased accumulator `B` (`acc + _` through `X => A`,
      `go(i, acc: B)`) where `oneBind(i: Int, acc: Int)` boxes nothing.
      METHOD: a ladder of control lanes in BuildShapeBenchmark, one rung
      per candidate — rowOneBind → a hand loop that unwraps `op(i, acc)`'s
      Mapped (the discarded pair only) → foldM (adds the generic step and
      its second closure) — and a boxing control, both lanes with a
      `case class Acc(n: Int)` accumulator so both box once a step; each
      lane its own `jmh-lane.sh` run, `-prof gc`, alternated. Then JFR
      allocation by class on rowFoldM vs rowOneBind as the independent
      reading. RESULT goes to specs/map-fusion.md (a Results section that
      corrects the "it is the step's two binds" line) and to history.d.
      ALSO in scope, cheap: the safe fused form's 21% (specs/map-fusion.md
      form 2) did strictly LESS work than master and read slower with only
      -5% bytes — check `resume`/`flatMap` bytecode size on 62654ffa7
      against FreqInlineSize 325 (`TestInlineBudget`, memory
      inlining-threshold-two-faces) before believing the number.
