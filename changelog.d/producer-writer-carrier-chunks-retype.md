## producer-writer-carrier-chunks-retype - Chunks[A] = Feed[Chunk[A]]

The operator confirmed the order reversal the previous lane proposed
("Chunks first"), and this is the retype: `type Chunks[A] =
Feed[Chunk[A]]` in okay-stream's Chunks.scala, every generator and
transformer emitting with `Writer.tell` and matching `Say` (the
`bound` cast the Bind payload needed is gone — the GADT refines the
type), `pull` on `Writer.uncons`, the fold/element/lazy walks and the
chunked `Source.merge` on `Stream[[W] =>> Unit ! Writer % W, Pure]`.
Outside that file: ParallelChunks (2 emits), Lex (1), Fs2Interop (1),
java Streams (4) — one `produce` → `tell` each. okay-cluster, persist
Streams, wroclaw and every other `Chunks.*` caller compiled unchanged,
as the survey said they would. Repo-wide `Test/compile` clean on
JVM+JS+Native; the four docs that named the old alias updated.

**Measured, before/after, three alternating rounds, JDK 21.0.12** (the
full table is in the spec's Results): `Chunks.fold` 2.533 → 2.647 us/op
(+4.5%), `foldLeft` +3.8%, map+fold +7.7%, `range`+foldLeft +7.6% — all
same-signed 3/3 and larger than the before-side spread; merge and the
StreamOps chunk pipelines within noise (+0.2%, +0.3%, +2.8%, −0.1%).
Allocation +16 B per told chunk per stage, exactly the `Say` node. So:
a real, bounded 4-8% on the tightest chunked folds, nothing measurable
where a chunk does real work. This corrects the previous lane's
"parity" — it was this same 4%, read from one shape.

The benchmark's Producer-carrier controls (`chunksFoldProducer`,
`chunksFoldLeftProducerDirect`, `chunksMapProducer`) cannot run
through `Chunks.*` any more; they are replaced by `chunksFoldChunks`,
`chunksFoldLeftChunksDirect`, `chunksMapChunks` on the new carrier,
with the old carrier kept as `Producer[Chunk[Long]]` in the `Probe`
own-method rows. `Chunks.foldLeftWriter`/`foldWriter` stay, for a
G-effectful `Source[Chunk[A]]`.

Files: okay-stream/src/main/scala/Chunks.scala, Source.scala,
scala-jvm-native/ParallelChunks.scala; okay-lex Lex.scala; okay-fs2
Fs2Interop.scala; okay-java Streams.scala; compare/src/jmh
ProducerWriterCarrierBenchmark.scala; src/jmh/history.tsv (5 rows);
specs/producer-to-writer-carrier.md, specs/chunked-streams.md,
docs/guide.md, docs/theory/07-logic-streams.md, README.md;
sprint.d/queue/producer-to-writer-carrier.md.
