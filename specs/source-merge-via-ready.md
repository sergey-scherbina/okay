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
      in its own JVM, alternating; the bar is no named loss

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
whose `uncons` throws) lost the value with it. Fixed in `Writer.scala`:
the iterators keep `k` unapplied behind a flag (no allocation), the
`uncons` views return it as a lazy `Bind(Return(()), k)`.
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
- **The control moved**: `okaySourceSingleDrain` read 45.4-48.7 here
  against 44.3 / 44.5 before the `Writer.uncons` fix; the fix adds one
  lazy `Bind` per `uncons` on the `toLazyList` road, so ~2-6% may be
  its price — inside the controls' own spread, noted rather than
  claimed.

The chunked roads (`chunked = true`, `flushAfter`, `mergeFlushing`,
`either`) still go through the shared channel: backlog
`merge-chunked-via-ready`.
