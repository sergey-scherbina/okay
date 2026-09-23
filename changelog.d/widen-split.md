## widen-split - `!.widen` is a coercion; the walk it was is `!.normalize`

The operator's question ("widen несёт двойственную нагрузку — может
стоит разделить?"), answered with a number. `!.widen[A, F, G](p)`
said "the same program in a wider row" and DID a walk — resume the
head, rebuild the tree node by node — on the strength of a comment
that credited the walk with `Source.merge`'s 5–7%; the history says
that number belongs to `Writer.widen`, the element-type re-tell, a
different function. The walk had already bitten once (the eager head
that started a stateful stage at widen time, windows-stage-rerun-
loses-pane). Now `!.widen` is `RowLift.into` — the one cast, sound by
erasure, nothing forced — and the walk keeps the name `!.normalize`
with its reason beside it; the signature is unchanged, so the 71 call
sites over 30 files compile as written. Measured
(`compare/WidenBenchmark`, a pure stage joined to the Async row,
alternated rounds on a shared box, the rounds a sibling matrix hit
discarded on the record): the coercion is the floor to the
microsecond (288.3 vs 289.5 µs / 10k) and to ±200 B in 4 MB; the walk
was +43% and +112 B per element. The chunked merge, whose
`Stage.unchunk` is joined by `!.widen`, reads 203.7 ± 4.0 at k=16
against the 2026-09-20 record 224.6 (~9%, with merge-lane-variance's
caveat); `okaySourceMerge` never used it. `TestWidenSplit` (core, 2:
the two names agree on six shapes; a deferred head is entered 0/1/2
times at widen/first run/second run); the deferred-head suites
unchanged. RowLift's `coerce` comment, theory ch. 4's "the upcast that
is not free", stage-pipeline's sixth door and the typepedia now say
which name walks. Landed as cb3a7af0.
