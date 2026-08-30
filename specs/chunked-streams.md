# Chunked streams

## Overview
Close the remaining gaps to chunked runtimes (ZIO/fs2/kyo on merge; plain
Iterator on pipelines) by amortizing per-element costs over chunks. The unit
of production and transport becomes a batch (an immutable indexed array);
the freer tree steps once per CHUNK, an element costs an array index. The
elementwise Stream layer is untouched — a chunked stream is an ordinary
`Producer[Chunk[A]]`, so every existing consumer (iterator, merge, Channel,
fold) works on it unchanged, because elements are polymorphic.

## Interface
- `type Chunk[+A] = scala.collection.immutable.ArraySeq[A]`
- `type Chunks[A] = Producer[Chunk[A]]` — a stream of batches
- `Chunks.generate(seed)(f)(g)(size = 64): Chunks[B]` — unfold with a tight
  per-chunk loop (a while over an array, no tree nodes per element)
- `Chunks.range(from, until, size = 64): Chunks[Long]`
- `Chunks.nats[N: Numeric](size)`, `Chunks.fibs[N: Numeric](size)`
- `extension (p: Chunks[A]) def elements: Iterator[A]` — the element view:
  one tree step per chunk, an index per element
- merge of chunked streams = the existing `Channel.merge` applied to
  `Chunks` values: one queue operation per chunk, no new plumbing

## Behavior
- [x] construction is lazy: an infinite `Chunks.generate` builds no chunk
      until pulled; `take(n)` on `elements` computes ceil(n/size) chunks only
- [x] `Chunks.range` emits a short tail chunk when size does not divide
- [x] `elements` agrees with the unchunked generator on nats/fibs
- [x] merge of two chunked ranges yields the union of elements
- [x] benchmark: chunked pipeline (map/filter/take/sum) at or under ~2x of
      the Iterator floor; chunked merge at or under ZIO's merge

## Chunked transformers (added 2026-08-30)
Transformers that stay chunk-in, chunk-out: each stage is a tight array
pass, no per-element Iterator plumbing, and the result is still a
`Chunks[A]` — so downstream consumers (merge, further stages) keep the
amortization. Spelled as `object Chunks` functions (`Chunks.map(p)(f)`,
like `Stream.map`): the postfix names belong to the monad (Free's map
transforms the ANSWER — the chunk), and extension-name overloads across
files are a known resolution trap.

- `Chunks.map(p)(f)`, `filter`, `take(n)`, `drop(n)`, `takeWhile`,
  `dropWhile` — lazy, one chunk at a time, deferred construction
- `Chunks.fold(p)(using Fold[A, S]): S` — the terminal: an inner while
  per chunk
- empty chunks after filter are skipped, take truncates the last chunk

Behavior:
- [x] transformer results agree with the LazyList reference pipeline,
      including chunk-boundary cases (take mid-chunk, filter-to-empty)
- [x] a transformer chain over an infinite source stays lazy
- [x] benchmark: measured 16.9 us — beats the elements view (23.6, -28%),
      lands 1.2x from the Iterator floor (14.1); kept as the fastest mode

## zip and rechunk (added 2026-08-30)
- `Chunks.zip(pa, pb): Chunks[(A, B)]` — pair elementwise across chunk
  boundaries: each emitted chunk is the overlap window of the two current
  chunks; ends at the shorter stream
- `Chunks.rechunk(p)(size = 64): Chunks[A]` — normalize chunk sizes
  (content unchanged, tail shorter): filter shrinks chunks, rechunk
  restores the amortization downstream

Behavior:
- [ ] zip realigns misaligned chunk sizes and stops at the shorter stream
- [ ] zip of infinite chunked streams is lazy under take
- [ ] rechunk preserves content and emits size-chunks with a short tail

## Out of scope
- chunk-size adaptivity
- changing the elementwise Stream/uncons doctrine

## Decisions
- **Chunking lives in the stream value, not the queue** — chosen because the
  queue-side experiment was REFUTED (2026-08-30, history.tsv
  abChunkedChannel): batching element-walked feeds into a chunked queue
  measured WORSE (median 150 vs 122 us) — buffer copies and consumer sync
  ate the win. ZIO's merge advantage is sources that are born chunked.
- **`Chunks[A] = Producer[Chunk[A]]`, not a new carrier** — the whole
  existing stream layer (iterator specialization, Channel.merge, Stream
  instances) applies verbatim; chunking composes by element polymorphism.
- **ArraySeq as Chunk** — immutable, O(1) indexed, wraps the generation
  array without copying (unsafeWrapArray; the array never escapes).

## Results
(2026-08-30, JMH f1 3+5, history.tsv rows cmpStreamOps-okayChunks / cmpMerge-okayChunks)
- pipeline map/filter/take(1000)/sum: **okayChunks 23.4 us** vs stdIterator
  13.9 — 1.7x from the floor (was 143 in elementwise-LazyList mode, 53 in
  iterator mode); kyo 239, zio 692, fs2 1410
- merge 2x500: **okayChunksMerge 14.7 us** vs zio 47.3 — **3.2x faster than
  ZIO** (was 158 elementwise); fs2 9031
- remaining per-element cost is the Iterator plumbing itself; per chunk: one
  freer-tree step and one queue operation
