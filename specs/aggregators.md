# P1 — The data-analysis algebra

## Overview
The core value proposition for data analysis: a named, typed, reusable
unit of aggregation, composable so that several statistics cost ONE
pass, honest about exact vs approximate, and aware of merge vs
un-merge. Design source: scalascript's aggregation-algebra spec (the
thinking is proven there); the implementation here is written fresh
and idiomatically (the user's decision), on our own Fold/Monoid/
Chunks machinery. Lives in the core (dependency-free).

## Interface
```scala
trait Group[A] extends Monoid[A]:
  def inverse(a: A): A                       // combine(a, inverse(a)) == empty

trait Aggregator[-In, Acc, +Out]:            // = Fold[In, Acc] + a projection
  def fold: Fold[In, Acc]
  def present(acc: Acc): Out
  def zip[In2 <: In, Acc2, Out2](that: ...): Aggregator[In2, (Acc, Acc2), (Out, Out2)]
  def map / contramap
```
- Standard library: count, sum, mean (= sum zip count, presented),
  variance/stddev (Chan/Golub/LeVeque merge form), min/max (any
  Ordering), first/last, topK, distinct (exact, then sketched),
  groupBy (Map[K, Acc] as a Monoid of merges).
- Terminals: `Chunks.fold(agg)`, `Stream.fold(agg)`, Foldable.
- **Sliding windows on Group** over our streams: the window state is a
  Group accumulator; aging data is SUBTRACTED (inverse), not
  recomputed — Chunks + Async ticks carry the window; a window over a
  non-Group aggregator is a compile-time-visible impossibility.
- **The distributed bridge**: any Aggregator exports the
  (zero, seqOp, combOp) triple — exactly what Spark/Flink aggregation
  APIs accept (this is P4's okay-spark leverage).

## P1b — Sketches (approximate aggregators, written fresh)
HyperLogLog (distinct count), Count-Min (frequencies), T-Digest
(quantiles): approximate MONOIDS with stated error bounds — exact and
approximate are two distinguishable, equally first-class kinds.

## Behavior
- [x] zip computes two statistics in one pass over one stream (probe:
      the source is consumed once)
- [x] mean/variance agree with two-pass references on random data
- [ ] group laws hold for sum/count (ScalaCheck); min/max are
      Monoid-only and the window rejects them at compile time
- [x] sliding window: subtract-on-age equals recompute-from-scratch
- [x] sketch error stays within stated bounds on generated data
- [x] (zero, seqOp, combOp) merges chunk-partial results associatively
      (the distributed contract, testable locally over Chunks)

## Out of scope
- rendering/reporting; sources (P4); adaptive windows
