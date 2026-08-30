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
- [ ] construction is lazy: an infinite `Chunks.generate` builds no chunk
      until pulled; `take(n)` on `elements` computes ceil(n/size) chunks only
- [ ] `Chunks.range` emits a short tail chunk when size does not divide
- [ ] `elements` agrees with the unchunked generator on nats/fibs
- [ ] merge of two chunked ranges yields the union of elements
- [ ] benchmark: chunked pipeline (map/filter/take/sum) at or under ~2x of
      the Iterator floor; chunked merge at or under ZIO's merge

## Out of scope
- chunked TRANSFORMERS (map/filter that rebuild chunks in place) — the
  element view through Iterator already fuses; revisit if measured
- rechunking, chunk-size adaptivity
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
(after implementation)
