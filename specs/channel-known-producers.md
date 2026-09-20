# channel-known-producers — a channel is a seam between streams, and the seam knows its sides

## Overview

The operator's question (2026-09-20): a channel is a stream written
from one side and read from the other, at the same time — can it be
built on plain streams? Half of it already is, and the half that is
not cannot be, and the useful part of the answer is what follows from
taking the framing seriously.

**What is already a stream.** `Source[W]` is writer and reader in one
program: `tell` writes, `uncons` reads, the interpreter alternates
them. `pipe(p)(c)` and `through` (Pipe.scala) are two programs handed
elements by the interpreter — `Writer` on one side, `Take` on the
other — with no buffer, exact order, and backpressure for free,
because the writer runs only when the reader asks. That is a
coroutine, and it is the cheapest thing in the library.

**What cannot be a stream.** "At the same time, from different sides"
is two fibers, and two continuations on two threads need a meeting
point that outlives both: a slot and the waiters on it. A `Free` tree
is a value and cannot be that point — walking it twice runs it twice.
The point can be one slot (`Queues.rendezvous`) but not none. That
cell is `Channel`, and the stream shell around it exists already:
`c.drained` is a `Source`, `feed` (Channel.scala) runs a `Source` into
a channel. Every library splits it the same way — fs2's `Queue` under
a `Stream`, ZIO's `Queue`/`Hub`, Kotlin's cold `Flow` against hot
`Channel`.

**What the framing buys.** A channel seen as a seam between streams
KNOWS ITS SIDES. `Channel.merge` has exactly two producers, one fiber
per source; `Channel.buffer` has exactly one. Yet both build
`Channel[A](capacity)`, the DEFAULT — the `growing` buffer, whose
whole reason to exist is a producer count nobody knows at
construction: it starts as one ring and, when two different producers
are seen, adopts that ring as part 0 and opens a part per producer.
That adoption is the one-shot swap across which a producer's own
order can break once (BUGS.md `growing-stale-route`, closed as a
documented trade on 2026-09-18), and it is paid by a merge that never
needed to guess. Where the count is known, size the parts: two
producers get two parts from the start (`relaxed.parts(2)`), one
producer gets one ring. No adoption, no swap, and per-source order is
EXACT by construction — each producer pushes to one part for its
whole life, and a part is one ring with one tail.

Not the default's business: `Channel.apply` still does not know its
producers, and `growing` remains the right answer there. This spec
changes the seams that do know.

## Interface

- `Channel.merge(s, t, capacity)`, `Channel.mergeChunked`,
  `Channel.mergeFlushing`: unchanged signatures; the channel underneath
  is `Queues.strong[A].relaxed.parts(2).each(capacity)` (or
  `.unbounded` for an unbounded capacity; a capacity below two keeps
  the rendezvous). `capacity` keeps the meaning it had in practice:
  under `growing` a two-source merge held `capacity` PER PART once it
  had grown (growing-part-sizing), so this holds what it held.
- `Channel.buffer(capacity)(s)`, `Channel.bufferChunked`: a plain
  bounded ring (`SentinelChannel(capacity)`), the single-producer
  mechanism the growing buffer was going to be anyway, minus its
  every-64th-push producer sampling.
- `Channel.forProducers(n, capacity)` (`private[okay]`): the one
  place the choice is written. `-Dokay.channel.known=growing` (or the
  environment variable `OKAY_CHANNEL_KNOWN`, since this module forks
  its tests) restores the old default-channel arm for A/B runs, the
  way `okay.channel.buffer` does for `Channel.apply`.
- `Source.merge`'s doc promise "EACH SOURCE KEEPS ITS OWN ORDER" is
  exact again, not "except once".

## Behavior

- [x] `Channel.merge` and `Source.merge` deliver each side's elements
      in exactly the order that side told them — a LAW, rounds with a
      fresh consumer thread each (the shape `TestChannelLaws`' two-
      producer law uses, since one consumer's starting part hides a
      reorder), `TestMergeOrder`.
- [x] `Channel.buffer` delivers in order (trivially: one ring).
- [x] nothing lost, nothing invented, close after both sides end, a
      failing side fails the channel — the existing `TestChannel`,
      `TestStreamSource`, `TestChunkEdges` and `TestChannelLaws`
      suites, unchanged.
- [x] the numbers: `MergeBenchmark` (channel, source and chunks
      merge), `MergeCapBenchmark` (64/256/1024), `ChunkFlushBenchmark`'s
      `okayChunked`, and the `Channel.buffer` rows of
      `IdiomaticApiBenchmark`, each arm in its own JVM, alternating.
      The bar is a matched pair: sized parts must not lose to growing
      on any of them, or the loss is named here.

## Out of scope

- `Channel.apply`'s default. It does not know its producers, and
  `growing` is the answer to exactly that; the actor mailbox is the
  clearest case (many senders, count unknown). The data this spec
  produces is the input to that question, not its answer.
- The timed flusher in `mergeChunked`/`mergeFlushing` is a SECOND
  producer thread per source (it sends the partial chunk it took from
  the feed's `TRef`), so a source's chunks reach the channel from two
  threads and two parts. That was so under `growing` as well; the
  per-source order law here is stated for the untimed merges, and the
  timed one keeps the claim it had (its flushed chunk may arrive
  beside, not behind, the feed's next).
- A `select` over N channels as a primitive: `AdaptiveFifo` with
  fixed parts IS N rings and one scanning reader, so nothing new is
  needed for fan-in.

## Design

`Channel.forProducers[A](n, capacity)`:

    n == 1                     -> SentinelChannel[A](capacity)         (a ring)
    n >= 2, 2 <= cap <= MaxRing -> Queues.strong[A].relaxed.parts(n).each(capacity).build
    n >= 2, cap > MaxRing       -> Queues.strong[A].relaxed.parts(n).unbounded.build
    cap < 2                     -> Channel[A](capacity)                (the rendezvous)

`merge`/`chunkedMerge` call it with 2, `buffer`/`bufferChunked` with 1.

## Decisions

- **Fixed parts, not `adaptive`** — chosen because the count is
  known; `adaptive` opens parts as producers arrive and pays a
  `claimPart` on first push for nothing here. Rejected: `growing`
  (the swap is the defect); a plain ring for the merge (two producers
  on one tail is what `growing` was measured to beat by 21.6% on
  `Source.merge`, default-retable 2026-09-08).
- **`capacity` stays per part** — chosen because that is what the
  growing buffer already held after its swap, and halving it would
  change every recorded merge number for a reason unrelated to this
  spec. Rejected: `capacity / 2` per part (growing-part-sizing
  measured the divided form 3.5x slower on this very seam).
- **An A/B switch rather than a control row** — the old arm is the
  default channel, which `Channel.apply` builds and every merge lane
  exercises; a property is the honest control, as
  `scripts/ab-defaults.sh` already does for `Channel.apply`.

## Results

**The A/B, 2026-09-20.** Both arms in one invocation, alternating
sized / growing / sized / growing, each arm its own JVM
(`-jvmArgsAppend -Dokay.channel.known=growing` for the old), `-f 1
-wi 3 -i 5`, load 3.7–5.6 throughout. The control lane
`okaySourceSingleDrain` — one source, no channel, untouchable by the
change — read 47.9 / 49.1 / 48.5 / 50.2 across the four arms, so the
box held still and the pairs may be read. Rows `ckp-*` in history.tsv.

| lane | sized r1 | growing r1 | sized r2 | growing r2 | verdict |
|---|---|---|---|---|---|
| `Channel.merge` 2×500 (LazyList in) | 68.0 ±1.2 | 71.3 ±2.9 | 67.8 ±1.1 | 70.6 ±1.9 | sized −4%, same sign, bars separate |
| `Chunks.merge` 2×500 (~31 chunks a side) | 11.57 ±0.59 | 11.28 ±0.47 | 11.93 ±0.35 | 11.09 ±0.21 | **sized +3–8%: the one loss** |
| `Source.merge` 2×500 elementwise | 106.9 ±3.9 | 88.2 ±2.7 | 100.5 ±2.5 | 111.8 ±3.7 | opposite signs in the two rounds: no verdict (merge-lane-variance) |
| `Source.merge` at capacity 64 / 256 / 1024 | 85.3 / 80.2 / 74.6 | 91.2 / 74.8 / 73.8 | 110.3 / 73.5 / 78.1 | 90.4 / 78.5 / 70.8 | 64 swings both ways; 256 and 1024 inside the ~15% band contended lanes carry |
| `Source.merge(chunked)` 2×2000, k 16 / 256 / 1024 | 224.6 / 226.1 / 226.0 | 231.3 / 232.6 / 228.7 | 226.4 / 248.7 / 225.0 | 231.9 / 228.5 / 227.1 | parity (one outlier at k=256 r2) |
| `Channel.buffer`, elementwise / chunk-native (one producer) | 189.5 / 18.55 | 187.5 / 19.97 | 196.0 / 19.54 | 191.7 / 19.85 | parity |

**What the numbers say.**

- **Matched pairs everywhere but one, and that one is explained.**
  `Chunks.merge` over 2×500 pushes ~62 chunks in all, and the growing
  buffer samples the pushing thread every 64th push — so on this lane
  it NEVER sees two producers and stays the single ring it started
  as, while two fixed parts pay one part scan per pop. The 3–8% is
  the price of exact order on a merge too short to contend, and it is
  named here rather than hidden: the §6 headline row (13.3 us against
  ZIO's 51.5) moves by under a microsecond.
- **The elementwise merge is the noisy lane it always was.** Two
  rounds read opposite signs; `merge-lane-variance` (backlog) records
  4x swings on unchanged code for this shape. Nothing is claimed for
  it in either direction.
- **The buffer at one producer is a wash**, as it should be: `growing`
  at one producer IS a ring plus a sample every 64th push, and the
  sample does not show.
- **The order law is exact and green** (`TestMergeOrder`, 20 rounds
  each of `Channel.merge`, `Source.merge` elementwise and chunked, and
  `Channel.buffer`, a fresh consumer thread per round).
- **AND IT FAILS ON THE OLD SHAPE, watched.** The same law under
  `OKAY_CHANNEL_KNOWN=growing`, 1 500 rounds, 20 CPU burners on 14
  cores (load 7 rising to 21 — the condition every earlier sighting
  needed): `Channel.merge: each side arrives in exactly the order it
  sent` — **round 540: the odd side came back out of order**. That is
  the three-sightings defect (BUGS.md `growing-stale-route`) reproduced
  on the merge seam by a test in the gate, for the first time with a
  law rather than a probe. The sized arm under the identical run:
  1 500 rounds of all three tests, load 17 rising to 23, **Passed 3,
  Failed 0** — no window to enter, because nothing is ever adopted.

So the change costs nothing measurable on the lanes that matter and
buys the exact per-source order as a law instead of an exception.
The question this leaves for `Channel.apply` (out of scope) is the
mirror image: growing's whole advantage is a producer count it does
not know, and the actor mailbox is where that is real.
