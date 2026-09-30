# Stream join by key — sort-merge on ordered streams, windowed on unordered ones

## Overview

Every join in the repository holds one side whole. `Bulk.join`
(okay-stream Bulk.scala) is the equi-join only, by contract
(specs/bulk.md), and its local instance is a hash join: the right side
into a HashMap, the left side streamed. `Tables.join` is the same
behind a plan node that turns the sides by estimated size. The Spark,
Flink and `java.util.List` instances delegate to their platform. The
demo's "stream join" (okay-demo Combine.scala) is a `merge` by readiness
plus a hand-written enrichment `Stage`, not a join by key. None of them
works on an unbounded `Source`, and nothing joins two `Chunks` without
materialising one.

This is the join by key OVER STREAMS, in two stages, both driven by the
DATA and never by the machine's clock:

1. **Sort-merge join** for streams already ORDERED by key (this lane):
   one cursor per side advancing the smaller key, a run of equal keys
   producing the cross product of that run, nothing held beyond the
   current run. Inner first; `left` and `full` answer `Option` on the
   missing side. It is `Source.zip` that skips — the same fiber-per-side
   shape and the same release law (specs/source-zip.md).
2. **Windowed join** for unbounded, UNORDERED streams (follow-on lane,
   Interface and Behavior written here so stage 1 leaves room for it):
   event time on the landed `Windows` machinery
   (specs/event-time-windows.md) — each side's rows kept per key until
   the watermark passes their interval, a row matched on arrival
   against what the other side's window holds, late rows COUNTED.

**Refuted in advance: a hash join on `Source`.** It is `Bulk.join` with
a fiber — the right side held whole — and the demo shows the
enrichment shape (`Stage` over one side, a lookup on the other) is
already written where somebody needs it.

## Interface

```scala
/** the machine, once: driven by two thin drivers below */
final class SortMerge[K, A, B, O](matched: (K, A, B) => O,
                                  leftOnly: ((K, A) => O) | Null,
                                  rightOnly: ((K, B) => O) | Null)
                                 (using Ordering[K]):
  /** decide everything decidable, emitting as it goes; then say what
   * the next decision needs: an element of one side, or nothing */
  def step(emit: O => Unit): SortMerge.Need
  def left(k: K, a: A): Unit
  def leftEnd(): Unit
  def right(k: K, b: B): Unit
  def rightEnd(): Unit
  /** the right run held now — the operator's live state, at most one
   * run of equal keys plus one lookahead element */
  def held: Int

object SortMerge:
  enum Need { case Left, Right, Done }

object Chunks:
  /** inner: every pair of left and right rows sharing a key */
  def joinSorted[K: Ordering, A, B](l: Chunks[(K, A)], r: Chunks[(K, B)]): Chunks[(K, (A, B))]
  /** left: every left row, `None` where the right side has no such key */
  def leftJoinSorted[K: Ordering, A, B](l: Chunks[(K, A)], r: Chunks[(K, B)]): Chunks[(K, (A, Option[B]))]
  /** full: every row of either side, `None` on the side that lacks the key */
  def fullJoinSorted[K: Ordering, A, B](l: Chunks[(K, A)], r: Chunks[(K, B)]): Chunks[(K, (Option[A], Option[B]))]

object Source:
  def joinSorted[K: Ordering, A, B](l: Source[(K, A)], r: Source[(K, B)], capacity: Int = 64)
                                   (using Scheduler, CanBlock, Wait, Pause): Source[(K, (A, B))]
  def leftJoinSorted[K: Ordering, A, B](l: Source[(K, A)], r: Source[(K, B)], capacity: Int = 64)
                                       (using Scheduler, CanBlock, Wait, Pause): Source[(K, (A, Option[B]))]
  def fullJoinSorted[K: Ordering, A, B](l: Source[(K, A)], r: Source[(K, B)], capacity: Int = 64)
                                       (using Scheduler, CanBlock, Wait, Pause): Source[(K, (Option[A], Option[B]))]
```

Stage 2, the windowed join (not built by this lane; the shape it
leaves room for):

```scala
object Source:
  /** rows of either side matched by key against what the other side's
   * window holds ON ARRIVAL; a row is held for `within` of event time
   * past its own `at`, evicted as the watermark (max seen minus
   * `lateness`) passes it; a row arriving behind the watermark is
   * dropped and counted, never joined */
  def joinWithin[K, A, B](l: Source[(K, A)], r: Source[(K, B)], within: Long, lateness: Long)
                         (atL: A => Long, atR: B => Long)
                         (using Scheduler, CanBlock, Wait, Pause): Source[(K, (A, B))]
```

## What a key-ordered stream promises

`joinSorted` takes each side to be NON-DECREASING in key under the
`Ordering` given. Equal keys may repeat on either side (a run); the
join of two runs is their cross product. The promise is CHECKED, not
assumed: a key smaller than the one before it on the same side fails
the join with an `IllegalArgumentException` naming the side and both
keys, at the row that broke it, after every pair told before it. A
join that silently skipped or misplaced rows on unordered input would
be the worst possible outcome — a wrong answer that looks like a
small one — and the check costs one comparison per row, which the
merge already pays.

## What happens to unmatched rows

- `joinSorted` drops them, on both sides.
- `leftJoinSorted` tells every left row: `(k, (a, Some(b)))` per
  matching right row, or `(k, (a, None))` once when the right side has
  no run at `k`.
- `fullJoinSorted` tells every row of either side: matched pairs as
  `(k, (Some(a), Some(b)))`, a left row with no right run as
  `(k, (Some(a), None))`, a right RUN with no left row as
  `(k, (None, Some(b)))` per element of the run — told when the run is
  released, that is when the left side has moved past `k` or ended.
- A right join is a left join with the sides swapped and the pair
  flipped; it is not a fourth function.

Order of the output: by key, non-decreasing, like the inputs. Within
one key: left rows in their order, each against the right run in its
order; a full join's unmatched right run comes after the matched rows
of the smaller keys and before anything of a greater key.

## Decisions

- **One machine, two drivers.** The merge is a pure state machine
  (`SortMerge`) that says what it needs next — a left element, a right
  element, or nothing — and emits through a callback. `Chunks` drives it
  with pulls on two cursors, one left chunk per tree step, the emitted
  rows buffered and told as one chunk; `Source` drives it with one
  receive per need on two buffered channels, the emitted rows told one
  by one. The logic that decides is written once and tested in
  isolation (a driver's test is then about its driving, not about
  merge cases). The stage-2 windowed join will be its own machine in
  the same shape.

- **Only the RIGHT run is held.** The ask said "a run of equal keys on
  both sides"; holding both is unnecessary. The right run is buffered
  until the left has moved past its key, and every left row streams
  against it — a left run of 10 000 equal keys costs nothing beyond
  the row in hand. `held` reports the live state, as `Windows.live`
  does, so the memory question is answerable rather than argued.

- **Companion functions, not extensions.** source-zip's lesson: an
  extension in okay-stream is a top-level name in split package `okay`,
  and `join` would collide with `Async`'s fiber `join` and every
  `Bulk.join` extension; the companion spelling `Chunks.zip(p, q)` /
  `Source.zip(s, t)` is what this joins.

- **`Source`: `zip`'s plumbing, copied, not shared.** The channel per
  side, the cancel scope entered in front and EXITED in the end
  branches, the survivor closed at the end — source-zip-lost-pairs paid
  for that shape and it is repeated here line for line rather than
  refactored out of `zip`, so this lane stays ADDITIVE (no existing
  body changed) and `zip`'s pinned tests keep pinning `zip`. If a third
  two-sided operator arrives, the three share a helper then.

- **Both sides are closed at the end, whichever ended.** A join that
  has answered `Done` closes both channels: an inner join ends when
  EITHER side ends (nothing left to match), so the other side's feeder
  may be parked on a full buffer and must be woken, exactly as `zip`'s
  survivor is. A left join ends when the left side ends; a full join
  when both do — the machine knows, the driver only asks.

- **Sortedness is checked, not assumed** (above). An `Ordering` is
  taken as a context parameter rather than `K <: Comparable`: the same
  key type joins under a reverse ordering if both sides were produced
  descending.

- **Stage 2 is event time on `Windows`' rule, not processing time.**
  Flink's interval join and Kafka Streams' KStream-KStream join both
  hold each side's rows for a bounded interval of EVENT time and match
  on arrival; the symmetric hash join (Wilschut & Apers 1991) is the
  same machine without the eviction. Held rows leave when the watermark
  passes them, the watermark is `Windows`' (max seen minus lateness,
  monotone), late rows are dropped and counted. The test is then a
  list, deterministic, like `Windows`' own.

## Behavior

Stage 1 (this lane):

- [x] `SortMerge`: inner join of two sorted lists is the same multiset
      of pairs as `Bulk.local.join` (a hash join) on the same input, in
      key order, for random sorted inputs with runs on both sides
- [x] `SortMerge`: a run of m left and n right rows at one key produces
      m × n pairs, left-major, and holds n rows while it does
- [x] `SortMerge`: the unmatched side per variant — inner drops, left
      answers `None` on the right, full answers `None` on either; a
      right run no left row reaches is told when released
- [x] `SortMerge`: an empty side — inner and left of an empty right are
      what they should be, full of an empty side is the other side
      wrapped
- [x] `SortMerge`: a key out of order on either side fails with the
      side and both keys named
- [x] `Chunks.joinSorted` agrees with `Bulk.local.join` on a chunked
      sorted input across chunk boundaries of different sizes; one
      output chunk per left chunk; lazy (a prefix of an endless sorted
      side)
- [x] `Chunks.leftJoinSorted` / `fullJoinSorted` on the same inputs
- [x] `Source.joinSorted` gives the same pairs as `Chunks.joinSorted`
      at buffer sizes 1, 4 and 64, each side on its own fiber
- [x] `Source.joinSorted` ends at either side's end and closes the
      other (the endless side stops producing); `leftJoinSorted` ends
      at the left's end; `fullJoinSorted` drains both
- [x] an early stop (`runFoldUntil`) on two endless sorted sources
      releases both sides once; a join that ran to its end releases
      nothing — on Loom and own
- [x] a side that fails fails the join after every pair told before it
- [x] a key out of order fails the `Source` join, after the pairs
      before it
- [x] docs: guide §6 example pinned (`TestDocExamplesStreamJoin`)

Stage 2 (follow-on, backlog `stream-join-windowed`):

- [ ] a row matches what the other side's window holds on arrival
- [ ] a held row is evicted as the watermark moves past `at + within`
- [ ] a row behind the watermark is dropped and counted, never joined
- [ ] the test is a list of timestamped rows, no clock

## Results

- stream-key-join (2026-09-30, stage 1): ADDITIVE — `SortMerge` new,
  three companion functions each on `Chunks` and `Source`, no existing
  body changed. Gate: `TestSortMerge` (9, JVM + JS + Native — it lives
  in okay-stream's `src/test/scala-cross`, since `src/test/scala` there
  is JVM-only), `TestSourceJoin` (6, JVM), `TestDocExamplesStreamJoin`,
  `TestDocSnippets`, then `affected master Test/compile` (303 module
  compiles, no warnings); recscan: the drivers' recursions are inside
  `defer`/`flatMap` lambdas, so the inventory did not grow.
- What the tests decided that the ask had not said: a row is matched
  only against a CLOSED run, so a right side that FAILS or breaks order
  while a run is still open loses that run's matches — the failure
  reached the run first (`TestSourceJoin`, the failing side: `(1, "y")`
  received but never told). A `Chunks` join tells the rows decided
  before an out-of-order key, including rows of GREATER key already
  decided (the row at 3 before the bad row at 2) — "after everything
  decided before it", not "after every smaller key".
- The inner join ends when EITHER side ends without asking the other
  side for another row (`TestSortMerge`, the empty-side case), so an
  endless right side under a finite left costs the run in flight, the
  buffer and the refused row — measured as at most 9 rows produced,
  the same bound `Source.zip`'s survivor test reads.
- okay2 has no port yet (source-zip's did land one): the machine is
  plain Scala and `okay2-stream` has `Chunks` and `Source`; filed with
  the stage-2 follow-on.
