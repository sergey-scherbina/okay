## producer-writer-carrier-pure-iterator - the stalled arc, unstalled: Chunks is Pure

The Producer→Writer arc had stopped twice in a row: `foldWriter` (built
in three lanes) turned out unusable at all three of its call sites
because it needs `CanBlock` and they build for JS too; and okay-cluster
"needs a combinator library that does not exist" before it can move.
Both had one root. `Chunks[A]` — and every one of those call sites
(`Bulk.scala:106`, `Pipeline.scala:92`, `Acceptance.scala:29`, all
`Chunks.fold` on a `Chunks[A]`) — is PURE: it walks
`Stream[Producer, Pure].iterator` under `Handler[Pure]`, which every
platform has. The fold prerequisite was being solved on the G-effectful
carrier, which needs a `Handler[G]`, for call sites that have no G.

The pure writer stream instance (`Stream[[W] =>> A ! Writer % W, Pure]`,
Writer.scala) was the only stream instance in the library with no
`iterator` override; the default `Iterator.unfold` paid an
`Option`+`Either`+`Free` node per chunk. It has one now — the twin of
`Stream[Producer, Pure]`'s in Generate.scala, no `split`, no
`TypeableK`, no `@nowarn`. Tested against `Writer.collect` as an
independent oracle on every tree shape (bare tell, right- and
left-nested binds, a mid-stream pure, 1M-element stack safety).

**Measured, 3 rounds, JDK 21.0.12 pinned, N=10000/64, `-prof gc`:**
`Chunks.fold`'s own loop over the pure Feed iterator, in an ordinary
method — what a retyped `Chunks.fold` IS — 2.63 us/op against library
`Chunks.fold`'s 2.53; +2,512 B/op = 157 chunks × one 16-byte `Say`.
PARITY, the spec's prerequisite for the `Chunks` retype met. The
"~2x, walking a Free tree at all" the earlier lanes recorded was the
loop written INSIDE the JMH benchmark method (4.8-5.0 there, halved by
moving it to its own method); stage 0's 2x was `Writer.fold`'s generic
per-chunk box. The same own-method probe does nothing for Producer
(4.5 in `compare` any way it is written; only okay-stream's compiled
`Chunks.fold` reaches 2.5) — recorded in
backlog.d/okay-core/chunks-fold-vs-foldleft-2x-gap.md, off the
migration's path.

**The plan's order was the other half of the stall.** "Leaves first,
Chunks last" had no leaves: okay-cluster (`Flow.Src` over `Chunks[A]`,
`Flow.map` = `Chunks.map`), persist Streams and wroclaw are typed ON
`Chunks[A]`; they cannot move before it (the bridge measured 1.5x
slower) and move for free with it — okay-cluster has zero direct
`produce`/`Produce` uses. The alias flip's blast radius is 14 main + 2
test files, not ~60. A reversal (`Chunks` first) is PROPOSED in the
spec's Decisions, pending the operator; the sprint item names the
retype as the next slice with the file list. Made false and fixed:
the sprint item's "next = okay-cluster" paragraph,
backlog `okay-cluster-flow-retype-needs-combinator-library` (removed —
its still-true measurement lives in the spec's Results),
`foldwriter-js-incompatible` (now says it is moot for `Chunks`).

Files: src/main/scala/Writer.scala (the override),
src/test/scala/TestGenerate.scala, compare/src/jmh/scala/okay/
ProducerWriterCarrierBenchmark.scala (6 rows + `Probe`),
src/jmh/history.tsv (5 rows), specs/producer-to-writer-carrier.md,
sprint.d/queue/producer-to-writer-carrier.md, backlog.d/okay-core/.
