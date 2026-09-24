## okay2-fast-channels - the ring-buffered channels in okay2-stream, and the default they make

okay-stream's channel mechanisms in okay2 (spec stage 28, docs §12):
`Buffer` and `Ring` (Vyukov's bounded MPMC, optional single consumer),
`Segments` (unbounded, no reclamation), `AdaptiveFifo` (parts per
producer, eager or lazy, adopting a ring as part 0 and reading it
first), `Growing` (a ring that partitions when two producers are seen,
never on one, not even through a parking channel); `SentinelChannel`
(termination as a mark in the buffer, decided after the claim),
`AbruptChannel`, `Queues` (strong/weak/composable/rendezvous). And the
default changes as in Scala 3: `Channel.apply` is a `SentinelChannel`
over `Growing` for a bounded capacity, over `Segments` past 2^20, and
`StmChannel` below two; `merge`/`buffer` build `forProducers` (two fixed
parts, one ring). `A | Null` is the bound `A >: Null`; the channels
store `Buffer[Any]` with one commented cast each. TestChannelLaws in
both tiers over eight implementations, TestRing, TestGrowing — plus a
LAW 1b the Scala 3 suite lacks: its law 1 let a decide-before-claim
mutant of `Ring.pushDeciding` through, and close racing four
non-parking producers catches it. `Flush`/`ParallelChunks` filed as
okay2-flush.
