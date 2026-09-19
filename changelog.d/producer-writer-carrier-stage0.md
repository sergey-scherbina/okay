## producer-writer-carrier-stage0 - measured, mixed verdict, stage 1 proceeds

`specs/producer-to-writer-carrier.md`'s stage 0 asked whether the
library's pure pull stream can move off `Producer[A] = A ! Produce`
(the identity signature — an operation IS its element, so `pure(a)`
type-checks as one and emits nothing, the bug that bit okay-watch)
onto the writer carrier `Unit ! Writer % W` — MEASURED, not assumed,
because Writer's own history (`12120c2a`) shed the same representation
at no cost on ITS loop, and that number is not evidence about
`Chunks`'.

`compare/src/jmh/scala/okay/ProducerWriterCarrierBenchmark.scala` runs
the A/B on `Chunks.fold`, `Chunks.map`, the `Source.ofProducer`/
`toProducer` bridges and a synthetic blob byte lane; the existing
`FoldConsumersBenchmark` supplies the pure elementwise pair unchanged.
Three rounds (host load 48.7 busy, then 2.7 quiet), every sign held
all three times:

- elementwise streaming (Async-shaped and pure) and chunked map+fold:
  the writer carrier wins, 14-24% faster, four independent
  measurements — two of them confirming `12120c2a`'s Say-node number
  on a fresh benchmark.
- `Chunks.fold` specifically: the writer carrier loses ~2x (5.175 vs
  2.527 us/op, N=10000/64). Diagnosed, not mysterious: `Chunks.
  foldLeft` dispatches `Fold.OfLong` once outside the loop and reads
  an unboxed `long` the whole way; `Writer.fold` dispatches on the
  TOLD type — `Chunk[Long]`, never `Long` — so it always takes the
  generic path, boxing the accumulator once per chunk. `Chunks.map`
  does not show this loss, because there is no accumulator to box.
- the bridges tax 75-90% over a direct fold, as deleting them in
  stage 2 promises to recover.
- the blob byte lane shows the same tax as the chunked-fold loss, same
  direction, too small to act on at 64 chunks with a `Unit`
  accumulator.

Decision: stage 1 proceeds — the loss is confined to `Chunks.fold`,
which is exactly the spec's own condition for it. Stage 2's `Chunks`
migration gains one prerequisite it did not have before: a chunk-aware
specialized fold for the writer carrier, measured at parity with
`Chunks.fold`, before `Chunks[A]` retypes. Until it exists, `Chunks`
stays on `Producer` as a documented exception.

Found and fixed along the way: `compare`'s `dependsOn` never picked up
`okayData`/`okayStm` after core-modularise moved `Sketch` and
`Stm`/`Tx`/`TRef` out of `okay`, so `compare/Jmh/compile` has been
broken on master since that migration, for anyone, unrelated to this
lane.

Full table, reasoning and the six `src/jmh/history.tsv` rows are in
the spec's `## Results`; `sprint.d/queue/producer-to-writer-carrier.md`
names stage 1 as the next claimable slice.
