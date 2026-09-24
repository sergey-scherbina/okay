## okay2-aggregate - the rest of Stream/Fold in okay2; okay2-stage2 closed

The Scala 3 core's Aggregate.scala in okay2: `Aggregator` with
`zip`/`map`/`contramap`, the unboxed `OfLong`/`OfInt`/`OfDouble` (each its
own fold), `zipLong`, `Sequential`, `mean`, `variance`/`stddev`,
`summary`, `min`/`max`/`first`/`last`, `topK`, `distinct`, `groupBy`, and
`sliding` on a `Group`. `sum[N]` keeps the specialization visible through
an implicit `SumOf` in place of the core's match type, with no cast. Also
`Foldable` with `foldTo`/`foldUntilTo`, streams' `zip`/`++`/`Stream.map`/
`Stream.flatMap`, and `MonadPlus[LazyList]` (specs/okay2.md stage 21).
21 tests.

That was the last of the operator's list, so the `okay2-stage2` backlog
item is closed. Of the core's files, only the Scala-3-specific ones
(`Member`, `Staged`) and `Prog`, declined by the minimal-by-default
decision, have no okay2 counterpart.

Docs: docs/okay2.md section 25.
