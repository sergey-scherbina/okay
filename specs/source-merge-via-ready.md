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

- [ ] DRAIN, THEN FAIL: a source that fails (a `Left` answer, a
      throwing `Run`, a throwing continuation) drops out of the ring;
      the others run to their end; then the merge fails with the FIRST
      failure. Nobody is cancelled for it. This is `Channel.merge`'s
      rule ("a failing source is recorded, the other still feeds"),
      which `Source.merge` users already had, and it is the library's
      general one: the consumer receives everything actually produced
      and only then hears that something broke.
- [ ] `Source.merge` over a failing side: the healthy side's elements
      all arrive, then the failure (a new law at the Source level; the
      channel-level one is `TestChannelFailure`)
- [ ] every existing `Source.merge` law still holds, unchanged:
      `TestStreamSource`, `TestMergeOrder` (each side's order exact),
      `TestMergeEnds`, `TestChunkEdges`, `TestPullBudget`
- [ ] the fibers still start at the FIRST PULL, not at construction
- [ ] `capacity` keeps its meaning: elements per side (what the two-part
      channel held per part)
- [ ] the numbers: `MergeBenchmark.okaySourceMerge` and
      `MergeCapBenchmark` (64 / 256 / 1024), new against old, each arm
      in its own JVM, alternating; the bar is no named loss

## Design

The old road stays reachable ONLY while it is measured:
`-Dokay.source.merge=channel` (read once) selects it for the A/B, as
`okay.channel.known` did for channel-known-producers. When the numbers
are in, the switch and the old road are deleted in the same lane — one
mechanism is the point.

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

(filled by the lane)
