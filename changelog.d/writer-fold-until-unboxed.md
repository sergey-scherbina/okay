## writer-fold-until-unboxed - refuted: the unboxed arms do not belong in the tree walks

The last unmeasured corner of specs/fold-until.md: `Writer.foldUntil`
and `Producer.foldUntil` skipped the `OfLong` dispatch on the guess
that the tree step dominates. Measured (`compare/
WriterFoldUntilBoxBenchmark`, 10k Longs told by a `Source.range`-shaped
program, per-lane gated JMH, `-prof gc`): the shipped generic walk
79.6 ± 3.1 µs / 128 B/elem; `FoldUntil.long` through the same walk
80.2 ± 9.1 and byte-for-byte the same allocation (the walk ignores the
shape, `final def add` boxes); the walk specialised to `OfLong` 73.1 ±
11.4 / 104 B/elem — the box is real, 24 B/elem, 19% of the bytes, and
invisible in time; and `Writer.fold(Fold.sumLong)`, the dispatch `Fold`
already has on this walk, is SLOWER than the generic `foldUntil` at
92.2 ± 2.2 (the `(S, A)` pair, no early exit). Where `Chunks` paid 25x
for the box, the tree pays a `Bind`, a `Say`, a continuation and a
`split` per element and the box drowns. Refuted as a lever; no arms
added; the benchmark stays as the record, the guess in the spec and
typepedia replaced by the numbers; what would move this walk is the
`split` itself (`typeablek-instanceof`). Landed as 75e01954.
