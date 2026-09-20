# channel-default-adaptive — does the bounded default still need to adopt a ring?

## Overview

The question channel-known-producers left (2026-09-20): what does the
BOUNDED default channel, `Channel[A](n)` for `2 <= n <= 2^20`, promise
— each producer's own order exactly, or "except once, across its one
swap"? Since that lane the growing default has ONE consumer in this
tree, the actor mailbox (`Channel[M](256)`; every other module site is
an unbounded `Channel[X]()`, which is `Segments` on one tail and never
grows), plus whatever bounded channels user code builds.

`growing` exists for a producer count nobody knows at construction:
a plain ring until two different producers are sampled, then the ring
is ADOPTED as part 0 and a part per producer opens. The adoption is
the one-shot swap across which a producer's order can break once
(BUGS.md `growing-stale-route`, a documented trade). `adaptive` is
the same buffer without the adoption — part 0 is fresh, producers
claim parts as they arrive, nothing is ever read out of a ring that
was somebody's before — and its per-producer order is exact by
construction.

`growing` was chosen over `adaptive` on 2026-09-08 when `adaptive`
was measured with the capacity DIVIDED among its parts (1 119 us at
one producer against the ring's 169, docs/queues.md); the same table
carries `adaptive` at per-part capacity ("16x memory") reading 169 /
166 / 115 at 1 / 4 / 16 producers against growing's 183 / 165 / 126 —
not behind on any row. And growing-part-sizing (the same day) made
`growing` hold per-part capacity too, lazily, so the memory that the
"16x" column was charged for is what the default already holds. The
conclusion was never redrawn from the corrected table. This spec
redraws it with a fresh A/B rather than from the record.

## Interface

No change until the numbers say so. The A/B uses the switch
`Channel.apply` already has (`-Dokay.channel.buffer=adaptive`, with
`-Dokay.channel.parts=8` so both arms carry the growing default's
eight parts). Lanes added: `ManyProducersBenchmark.default_elem` and
`default_chunk` — `Channel[Long](Cap)` under the existing producers
param — since every lane there names its mechanism explicitly and
none goes through the default.

If matched: `Channel.apply`'s bounded arm becomes
`Queues.strong[A].adaptive.parts(Parts).each(capacity)`, the exact
per-producer law returns to `TestChannelLaws` for the default
(`swapsItsBuffer` empties), the actor mailbox inherits it, and
`Growing` with the adoption path in `AdaptiveFifo` (`first`,
`adopted`, `popManyAdoptedFirst`) becomes deletable — a follow-up
lane, since deleting is its own diff.

## Behavior

- [ ] the A/B: `ManyProducersBenchmark.default_elem`/`default_chunk`
      at 1 / 2 / 4 / 16 producers, `ActorReactiveBenchmark.actorTell`,
      `actorTellMpmc`, `actorTellBacklog`, `actorAsk`; each arm its own
      JVM, alternating, two rounds; a control lane that builds its
      mechanism explicitly (`oneRing_chunk`, `plainSource`) and must
      hold. The bar: `adaptive` must not lose beyond the contended-lane
      band (~15%, docs/benchmarks.md §6) on any row, and the
      one-producer rows are the ones that decide — that is where
      partitioning from the first push was priced at 19%.
- [ ] a verdict written here either way.

## Out of scope

- Deleting `Growing` (a follow-up lane if the switch happens).
- `Channel.apply`'s unbounded arm (`Segments`, one tail) and the
  rendezvous below capacity two.

## Decisions

- **Measure through the default's own switch, not a control row** —
  the arm IS the default, and `scripts/ab-defaults.sh` already
  measures `Channel.apply` this way. `parts` pinned to 8 on the
  adaptive arm so the two buffers differ in adoption only.

## Results

(filled by the lane.)
