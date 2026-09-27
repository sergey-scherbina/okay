# source-merge-via-ready — one merge mechanism

## Overview

`Source.merge` joined two sources through ONE shared channel: a fiber
per side, both pushing into a two-part ring (`Channel.merge`,
specs/channel-known-producers.md), read back by one scanning reader.
`Source.mergeReady` (specs/ready-merge.md) joins sources by stepping a
ring of their own continuations. Measured on the matched pair — each
side still on its own fiber, into its own ring, joined by the
ready-merge — the ring join read 0.86x / 0.93x of `Source.merge` on
2x500, bars separate in both rounds, and the tighter lane
(ready-merge-numbers, 2026-09-26). The operator's intent from the
start: ONE merge mechanism, with parallelism a property of the source
rather than of the merge.

So the elementwise `Source.merge` becomes exactly that composition:

    s merge t  ==  mergeReady(buffer(capacity)(s).drained, buffer(capacity)(t).drained)

and `mergeReady` takes the failure rule `merge` always had.

## Interface

- `Source.merge(t, capacity, chunked = false, flushAfter = None)`:
  unchanged signature. The `chunked = false` road is the composition
  above; the `chunked = true` roads (timed flush, `mergeFlushing`,
  `either`) are unchanged in this stage — they are a different lane
  (chunks through a channel) and were not what was measured.
- `Source.mergeReady`: the failure rule below.

## Behavior

- [x] DRAIN, THEN FAIL: a source that fails (a `Left` answer, a
      throwing `Run`, a throwing continuation) drops out of the ring;
      the others run to their end; then the merge fails with the FIRST
      failure. Nobody is cancelled for it. This is `Channel.merge`'s
      rule ("a failing source is recorded, the other still feeds"),
      which `Source.merge` users already had, and it is the library's
      general one: the consumer receives everything actually produced
      and only then hears that something broke.
- [x] `Source.merge` over a failing side: the healthy side's elements
      all arrive, then the failure (a new law at the Source level; the
      channel-level one is `TestChannelFailure`)
- [x] every existing `Source.merge` law still holds, unchanged:
      `TestStreamSource`, `TestMergeOrder` (each side's order exact),
      `TestMergeEnds`, `TestChunkEdges`, `TestPullBudget`
- [x] the fibers still start at the FIRST PULL, not at construction
- [x] `capacity` keeps its meaning: elements per side (what the two-part
      channel held per part)
- [x] the numbers: `MergeBenchmark.okaySourceMerge` and
      `MergeCapBenchmark` (64 / 256 / 1024), new against old, each arm
      in its own JVM, alternating. The bar was "no named loss"; ONE is
      named (capacity 256, ~10%, two causes refuted, backlog
      `merge-cap256-gap`) and landed with the operator's ask for one
      mechanism, since the default capacity is faster

## Design

The old road was reachable ONLY while it was measured
(`-Dokay.source.merge=channel`, as `okay.channel.known` was for
channel-known-producers) and was deleted with the switch in this lane
— one mechanism is the point.

`ReadyMerge`'s failure: a per-run `failure` cell (first wins). A
source's `resume` is wrapped so that a throw from its continuation is
caught and recorded; a `Run` that throws, and an `Await` answered
`Left` (in place or later), are recorded the same way; the failed
source is dropped (`live -= 1`) and never touched again. When `live`
reaches 0 the merge ends by throwing the recorded failure, if any.

## Decisions

- **Drain-then-fail over fail-fast** — fail-fast was `mergeReady`'s
  first rule and cancelled the parked sources; it would have taken
  from `Source.merge` users the healthy side's remaining elements,
  which `TestChannelFailure` and typepedia's `Channel` entry name as the
  point. The price is the one `Channel.merge` already paid: a failure
  beside an endless source is never reported. Rejected: a flag
  choosing between the two rules — two rules for one combinator is
  what this lane removes.
- **The chunked roads wait** — their fixed-size chunking and timed
  flush live in the channel's feed; moving them means a
  `bufferChunked` with a flusher per side, a separate measurement
  against `ChunkFlushBenchmark`, and `merge-lane-variance`'s known
  noise on exactly that lane. Filed when this lands.

## Results

**Found on the way: a told value was lost when the next step threw
— in the CORE, and the old `Source.merge` had it too.** The new
Source-level failure law came back `1, 2, 10, 20` for a side that told
1, 2, 3 and then failed. Isolated by probes: every view of a writer
as a stream — `Writer.uncons` (pure and with effects) and both linear
iterators, which `Channel.buffer`'s `feed` walks — applied the
continuation `k(())` BEFORE handing the told value over, so a source
whose next step throws while being BUILT (`Source.of` over a stream
whose `uncons` throws) lost the value with it. Fixed in `Writer.scala`
by `Writer.toldThen`: `k` is still applied as the value is handed over,
but a throw from it becomes the REST — a `Delay` that throws when next
stepped — so the value is out first. FIRST CUT, REVERTED: applying `k`
lazily (a flag in the iterators, a `Bind(Return(()), k)` from `uncons`)
also fixed the loss but moved WHEN a source's code runs, and two
FoldUntil laws that count pulls by exactly that went red
(`TestFoldUntilStreams`: `pipe` pulled 2 where `Writer.foldUntil`
pulls 3). The kept fix changes nothing on a path that does not throw.
`TestWriterToldBeforeThrow` (core, watched failing first) and
`TestSourceToldBeforeThrow` (okay-stream, through `Channel.buffer`).

**The numbers** (`src/jmh/history.d/…-source-merge-via-ready.tsv`),
2x500, each arm its own JVM, alternating, jmh-lane through
bench-window; contaminated and noisy tries were discarded and retried
by the script:

| lane | new (ring join) | old (shared channel) | |
|---|---|---|---|
| `okaySourceMerge` (cap 64) | 98.4 / 104.7, later 103.6 / 105.2 | 107.3 / 108.9, later 115.4 / 111.3 | **0.92-0.96x** |
| `MergeCapBenchmark` cap 64 | 103.4 / 103.7, later 102.5 / 100.7 | 111.7 / 109.4 | **0.93-0.95x** |
| cap 256 | 89.6 / 88.2, later 87.7 / 85.1 | 80.4 / 81.6, later 79.2 / 73.6 | **1.08-1.16x — THE NAMED LOSS** |
| cap 1024 | 78.0 / 78.0 | 76.9 / 78.3 | parity |

- **The default (capacity 64) is faster**, which is what every
  `merge` call without an explicit capacity gets.
- **Capacity 256 loses ~10%**, and two explanations were REFUTED
  before it was named: one element per turn (a quantum of a batch,
  64, recovered 2-3% everywhere — kept — and left the gap), and the
  per-side buffer's type (`relaxed.parts(1)`, the old merge's own part
  type, read 87.1 / 95.8 against `SentinelChannel`'s 87.3 / 89.8).
  The old arm at 256 is itself unstable — ±9 in 5 of 6 attempts, the
  script gave up on one round — so part of the gap may be the old
  road's good forks. Backlog `merge-cap256-gap`.
- **The control** (`okaySourceSingleDrain`) read 45.4-48.7 in these
  rounds against 44.3 / 44.5 the day before; those rounds ran the
  REVERTED lazy cut, which added a `Bind` per `uncons` on that lane's
  road. The kept `toldThen` adds nothing where nothing throws.

The chunked roads (`chunked = true`, `flushAfter`, `mergeFlushing`,
`either`) still go through the shared channel: backlog
`merge-chunked-via-ready`.

**The 256 gap, CLOSED the same evening (merge-cap256-gap).** Ten
forks per arm showed two things the two-fork rounds hid: the OLD
road has pathological forks (one round at cap 256 ran 81..725 us,
half of them 5-10x slow; at cap 64 up to 1046), which the new road
never showed; and in its good forks the old road really was ~16%
faster at 256. `-prof gc` named why: ~65 B more per element, because
every element was told TWICE — once by the side (`Writer.of(Drain)`:
a `Some`, a tuple, a `Drain` copy and the nodes around them) and again
by the merge (a fresh `Say`, `Inject`, `Bind` and a lambda). Fixed:
`drained` is a hand loop over the batch (an element costs its tell and
one Bind), and the merge forwards the source's own `Inject(Say)` node
with ONE shared continuation. After, median of 5 forks, two rounds:
cap 64 **0.81-0.82x** of the old road, cap 256 **0.91-0.93x**, cap
1024 **0.77-0.85x**, `okaySourceMerge` **0.73-0.82x**, and 12-14% fewer
bytes per operation. No named loss remains.

**The chunked roads, TRIED and REVERTED (merge-chunked-via-ready,
2026-09-27).** A chunk channel per side (its own `feedChunked` or
`feedFlushing`, its own flusher, two parts when windowed) joined by
`mergeReady` and unchunked by `Writer.expand`, against the shared
chunk channel, on `ChunkFlushBenchmark` (`okayChunked`,
`okayChunkedFlush`, `okayChunkedFlushShort`, k = 16/256/1024), 5 forks
per arm, two rounds: no win anywhere, and 1.2x slower in half the
forks — the ring road's forks were BIMODAL (~195-210, the old road's
level, or ~246-266), the old road's steady at ~200-215. Rows in
`src/jmh/history.d/…-merge-chunked-via-ready.tsv`. The chunked roads
stay on the shared channel: one mechanism for the elementwise merge,
and a measured reason the batch road differs. Kept from the attempt:
the flusher as one helper (`flusherFor`), and a real fix it found —
a chunking feed that FAILED dropped its partial chunk, so
`Source.merge(chunked = true)` and `Channel.mergeChunked` over a
source telling 1, 2, 3 and then failing delivered the other side and
the failure but none of the three (`failAfterTail`; two laws in
`TestChannelFailure`, both watched red on master first).

**Why the ring is slow with chunks — ANSWERED (ring-chunk-bimodal-forks,
2026-09-27).** Per-fork counters on the reverted road (built from
f0f355bd4): the slow forks are not a JIT outcome (raising
`FreqInlineSize` or `MaxInlineLevel` leaves them) nor thread placement
(one carrier makes every fork 2x slower, without modes), nor the merge's
own parks (a bounded spin before parking removes those — 0.0 per op at
20 000 checks — and the slow forks stay). They are the SIDES' parked
receives: 84-96 (up to 217) async wakes per op in a slow fork against
11-20 in a fast one, 206-213 awaits per op against 73-95 for 250 chunks.
Two self-sustaining regimes: PRODUCERS AHEAD — chunks pile up in each
side's channel and the merge takes them in batches, synchronously — or
CONSUMER CAUGHT UP — each side's channel runs empty, its receive parks,
and the channel hands the next send over as ONE element, the wake-up
(callback, wake queue, waker) running on the PRODUCER's thread inside
its send, which slows the producer and keeps the consumer caught up.
The early timing of a fork picks the regime. The shared channel of the
old road rarely runs empty — two producers feed one queue — so it stays
in the first. A fix is a design change, not a knob: a STANDING receiver
per side that accumulates sends into a buffer of its own, instead of a
one-shot park per element (backlog `ring-standing-receiver`).


## Stage: a notifying receive for the ring's sides (ring-standing-receiver + ready-merge-chunk-forward, 2026-09-27)

The one lane the operator unpaused: first the standing receiver, then —
only if it removes the slow regime — the chunked roads onto the ring.

**The receive.** `SentinelChannel.receiveManyOrWatch(max)(k)`: what is
buffered is answered at once as a chunk (or the end); when nothing is,
a waiter is registered whose wake-up only NOTIFIES — `k(Right(null))`,
"look again" — and the elements stay in the ring. A send to an empty
side therefore does no receive work on the producer's thread (no pop,
no hand-over of ONE element); the side's reader takes EVERYTHING
buffered when the merge's drive comes back to it. Re-armed only when
that take finds nothing. `drained` reads through it; any other channel
falls back to `receiveManyAsync`.

- [ ] order: a side's elements arrive in the order sent, across
      notify / take cycles
- [ ] end: a close while the watch is armed notifies, and the look
      that follows answers the end (after everything buffered)
- [ ] failure: a failed channel answers its failure once drained,
      through the watch as through `receiveManyAsync`
- [ ] cancel: a cancelled watch is never called, and the element that
      would have woken it is still received by the next reader
- [ ] the existing laws unchanged: TestChannel*, TestSentinel*,
      TestReadyMerge*, TestSourceMerge*/TestMergeOrder/TestMergeEnds,
      TestChannelFailure, TestDrain
- [ ] THE REGIME CHECK: the chunked ring road (f0f355bd4's, rebuilt)
      over `okayChunked`, -f 10, one-shot receive against the notifying
      one, slow forks counted and the per-fork side wakes read. If the
      slow regime stays, STOP: stage 2 is not run.

**Stage 2 (conditional).** `Source.merge(chunked = true)`,
`mergeFlushing`, `either` onto `ReadyMerge[Chunk[A]]` over a chunk
channel per side plus `Writer.expand`; BAR: no arm slower than the
shared channel at any k on `ChunkFlushBenchmark`, no bimodality; then
the shared-channel chunked road is deleted.
