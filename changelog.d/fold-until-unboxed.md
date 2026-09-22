## fold-until-unboxed - the four unboxed `FoldUntil` shapes, after the measurement that earns them

The last open item of specs/fold-until.md, done the way `Fold.OfLong`
was: measured first. `compare/FoldUntilBoxBenchmark` (10k Longs in
chunks of 64, summing into a Long, the fold as data, the stop never
firing), three rounds with the third on a quiet box: the same loop
with the state declared `long` is 25–27x faster than the generic
`FoldUntil` (0.99 vs 26.4 µs), and on the shipped path
`FoldUntil.long` through `Chunks.foldUntil` reads 7.5 ± 1.0 against
the generic 20.7 ± 1.1 — 0.36x, parity with `Fold.sumLong` (7.4). The
`done` branch per element is not measurable: +11% in round 1, −22% in
round 3, both inside the boxed `Fold`'s bars. Round 2 recorded and
discounted (a sibling's sbt started mid-run; the loop lane read
64 ± 30). All rows in src/jmh/history.tsv as `fuu-*`.

So: `FoldUntil.OfLong`/`OfInt`/`OfDouble`/`OfBoolean` (Fold.scala,
`initX`/`addX`/`doneX`/`endX` — the predicate declared at the
primitive too, or the state would box on its way into it), the
inline builders `FoldUntil.long(z)(f)(stop)(finish)` and siblings,
`exists`/`forall` as `OfBoolean`, and the dispatch in
`Chunks.foldUntil`, `Stream.foldUntil` and `Foldable.foldUntil`.
`Writer.foldUntil`/`Producer.foldUntil` do not dispatch — their cost
is the tree step, unmeasured, trigger stated. `TestFoldUntil` +1,
`TestFoldUntilStreams` +1 (the four arms agree with the generic one
on every walk and stop at the same chunk). Typepedia and guide §3
carry the number. Landed as 5bcef2b7.
