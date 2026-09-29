# Source.zip — lockstep on the live carrier

## Overview

`zip` existed on every synchronous carrier and on none of the live ones.
`Chunks.zip(pa, pb)` pairs elementwise across chunk boundaries
(specs/chunked-streams.md), `Stream.zip` is `toLazyList.zip` — a
convenience — and `Gen.zip` is the one lockstep zip that materialises
nothing (specs/strymonas-zip-fusion.md). `Source` had `merge`, `concat`
and `either`, and `Channel.scala` names zip as the shape merge is NOT
("the concurrency zip and ++ cannot express"): merge answers whichever
side is ready, zip needs both. This is that operator, in `merge`'s own
shape, and the `Chunks.zip` extension so a chunk stream reads
`p zip q` like the rest of its API.

## Interface

```scala
extension [A](s: Source[A])
  /** pairs in lockstep until EITHER side ends; each side on a fiber of
   * its own, `capacity` elements buffered a side */
  infix def zip[B](t: Source[B], capacity: Int = 64)
                  (using Scheduler, CanBlock, Wait, Pause): Source[(A, B)]
  def zipWith[B, C](t: Source[B], capacity: Int = 64)(f: (A, B) => C)
                   (using Scheduler, CanBlock, Wait, Pause): Source[C]

extension [A](p: Chunks[A])
  infix def zip[B](q: Chunks[B]): Chunks[(A, B)]      // = Chunks.zip(p, q)
```

## Decisions

- **`merge`'s shape, not a new one.** Each side is `Channel.buffer`ed
  onto a fiber of its own and the pairing runs on the consumer's thread
  of control: one receive per side per pair. The two sides' pulls
  overlap through their buffers, which is what a fiber per side buys;
  the consumer itself is sequential, which is what lockstep means.
  The receive is spelled as `Channel.drained` spells its await — an
  `Async.Await` node built on the zip's own row — because `receive`
  answers `! Async` alone and the row here also carries the Writer.
- **Not on `mergeReady`'s ring.** Readiness is the wrong question:
  a zip waits for the slower side whatever the other has ready, so a
  ring of continuations polled by readiness would only spin on the
  side that is not the bottleneck.
- **The survivor is closed at the end, not left to the scope.** When
  one side answers `None`, the other side's channel is closed on the
  spot: its feeder, parked on the full buffer, wakes and ends, and the
  elements it had buffered are dropped — a zip has no use for an
  unpaired element. A feeder still inside its source's own `Await`
  ends at the next element it offers; a channel's close does not reach
  into a source's pull, for `merge` either. Because both channels are
  closed by the time the program ends normally, the cancel scope's
  release (`Merge.closing`, now `private[okay]`) finds nothing open
  and counts nothing — the same accounting `Merge.Shared.elements`
  has, and the same `mergeReleases` counter the tests read.
- **The scope is entered in front and never exited**, as
  `Merge.Shared.elements` does and for its reason: an exit after the
  loop is a Bind over the whole element program, a rotation per pair.
  The drive releases it when the program ends, early or not.
- **Failure after what was buffered.** `Channel.fail` keeps the
  elements the feeder had already pushed and `receive` answers them
  before the failure, so a failing side fails the zip at the pair its
  failure reached and every pair before it is delivered — `merge`'s
  promise, inherited rather than re-implemented.
- **The okay2 port, without the early-stop release.** The first cut of
  this spec said okay2 had no `Source` or `Chunks` and dropped the port;
  wrong — `okay2/okay2-stream` has both (Source.scala, Chunks.scala), and
  the port landed with the lane: `SourceOps.zip`/`zipWith` and
  `ChunksOps.zip`, the same pairing loop, the survivor closed at the
  end. What okay2 cannot port is the EARLY-stop release: it has no
  cancel scope for a drive to release, and its `merge` leaves both
  feeders parked on their bounded buffers too. Its zip says so.
- **The feeder's death is proved on Loom.** The feeder-ends test reads
  `!thread.isAlive`, which proves a fiber ended only where a fiber IS a
  thread. Since scheduler-default-flip (fd6eba1d8) the default is
  adaptive, whose fibers run on pooled workers — the test went red on
  a rebase with no change to zip. It is pinned to `Schedulers.loom`, and
  a second, scheduler-free proof sits beside it: the endless side has
  stopped producing. The early-stop release test runs on loom, own and
  the default.

## Behavior

- [x] lockstep pairs, ending at the shorter side whichever side that
      is, at buffer sizes 1, 4 and 64; an empty side gives no pair
- [x] each side keeps its own order however the sides' buffers align
      (a counter source against a LazyList source, capacity 7, 2000
      pairs all (i, i))
- [x] `zipWith` folds the pair as it is told
- [x] two infinite sources zip lazily under an early stop
      (`runFoldUntil` take 5), which releases both sides once; a zip
      that ran to its end releases nothing — on Loom and on `own`
- [x] the side that outlives the other is closed at the end: its
      feeder, parked on the full buffer, ends (the feeder's virtual
      thread is joined and found dead) and produces at most the buffer
      plus the refused element
- [x] a side that fails fails the zip, after every pair told before
      the failure
- [x] `p zip q` on `Chunks` equals `Chunks.zip(p, q)`
- [x] okay2: the same laws (TestSourceZip, TestChunks) — lockstep at
      every buffer size, order, zipWith, lazy early stop, the survivor
      closed, failure after the pairs before it

## Results

- Additive lane (source-zip, 2026-09-28): new methods only, no
  existing body changed; `Merge.closing` widened to `private[okay]`.
  Taken over 2026-09-29 by another session (the first stopped
  uncommitted on 2026-09-28): the Loom pin, the okay2 port.
  Gate: the lane's suites plus `affected master Test/compile`.
