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

- [x] the A/B: `ManyProducersBenchmark.default_elem`/`default_chunk`
      at 1 / 2 / 4 / 16 producers, `ActorReactiveBenchmark.actorTell`,
      `actorTellMpmc`, `actorTellBacklog`, `actorAsk`; each arm its own
      JVM, alternating, two rounds; a control lane that builds its
      mechanism explicitly (`oneRing_chunk`, `plainSource`) and must
      hold. The bar: `adaptive` must not lose beyond the contended-lane
      band (~15%, docs/benchmarks.md §6) on any row, and the
      one-producer rows are the ones that decide — that is where
      partitioning from the first push was priced at 19%.
- [x] a verdict written here either way.

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

**Verdict (2026-09-25): NOT matched — the default stays `growing`.**
`adaptive` loses beyond the bar on the row that decides it, and the
row is the one the spec did not expect: TWO producers, not one.

Method as specified, with three corrections the run itself forced:
the arm printed by the benchmark's own `@Setup` (the fork received
`buffer=adaptive parts=8` / `buffer=growing`, checked, not assumed);
one lane per `Jmh/run` through gate.sh, the arms alternating; and, from
15:06 on, a lane accepted only when its error was <= 10% of its score,
after the first round showed a quiet-before/quiet-after check lets a
sibling's gate start mid-lane (+-60% rows). The box sat at load 30-80
all afternoon with a VM at ~600% (excluded from the quiet check: it
loads both arms alike). `consumers` pinned to 1 — the mailbox's shape.
Rows (us/op, adaptive / growing), src/jmh/history.d/*-channel-default-adaptive.tsv:

| lane | adaptive | growing | ratio |
|---|---|---|---|
| default_elem p=1, r1 / r2 | 493 / 501 | 586 / 559 | 0.84 / 0.90 |
| default_chunk p=1 | 142 | 189 | 0.75 |
| **default_elem p=2, r1 / r2** | **460 / 433** | **364 / 372** | **1.26 / 1.16** |
| default_elem p=4 | 292 | 311 | 0.94 |
| default_elem / chunk p=16 | 203 / 117 | 249 / 152 | 0.82 / 0.77 |
| actorTell / Mpmc / Backlog / Ask | 13542 / 13286 / 9632 / 2009 | 13603 / 13860 / 10026 / 1957 | 1.00 / 0.96 / 0.96 / 1.03 |
| controls oneRing_chunk / plainSource | 120 / 64 | 125 / 65 | 0.96 / 0.99 (held) |

- The one-producer rows, which the spec named as the ones that
  decide, are WINS for adaptive (0.75-0.90): partitioning from the
  first push costs nothing measurable at one producer now.
- Two producers is where adoption pays: `growing` runs one ring until
  the second producer arrives and then one swap, while `adaptive`
  partitions from the first element. 16% and 26% in the two
  alternating rounds, both past the ~15% contended-lane band.
- `growing`'s `default_chunk` at p=2 never measured under 10% error in
  ten attempts, and read 795 +- 346 in the one early run. Followed up
  the next night (growing-two-producer-variance): the cost IS bimodal,
  but per JVM FORK, not per run, and NOT `growing`'s — a fork is either
  fast (~150-230 us/op) for all its iterations or 4-6x slower for all of
  them, and it happens to BOTH buffers at p=2: growing 1/4 then 0/12
  forks, adaptive 0/4 then 4/12. The accepted p=2 rows above were tight
  (5-8% error), so no slow fork sits in them and the verdict stands; a
  2-producer lane with few forks should be read with this in mind.
- Round 2 of the p=4/16 and actor rows was not taken: the p=2 row
  decided the question, and the box was needed by siblings.

So the exact per-producer law does not return to the default, and
`Growing`'s adoption path is not deletable; `adaptive` stays an arm of
the switch, and `merge`/`buffer` keep building for their known
producers (channel-known-producers).
