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

- [ ] `Channel.merge` and `Source.merge` deliver each side's elements
      in exactly the order that side told them — a LAW, rounds with a
      fresh consumer thread each (the shape `TestChannelLaws`' two-
      producer law uses, since one consumer's starting part hides a
      reorder), `TestMergeOrder`.
- [ ] `Channel.buffer` delivers in order (trivially: one ring).
- [ ] nothing lost, nothing invented, close after both sides end, a
      failing side fails the channel — the existing `TestChannel`,
      `TestStreamSource`, `TestChunkEdges` and `TestChannelLaws`
      suites, unchanged.
- [ ] the numbers: `MergeBenchmark` (channel, source and chunks
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

(filled by the lane.)
