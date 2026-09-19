# producer-to-writer-carrier — the element in the type

## Overview

`Producer[A] = A ! Produce` with `Produce = Id` (src/main/scala/Generate.scala)
is the library's pure pull stream: an operation IS the element it
emits. That representation has one consequence the type system cannot
see and two the code pays for. The element type is the ANSWER type,
so `pure(a)` type-checks wherever `produce(a)` does and emits NOTHING
(a producer's `Pure` is its end) — measured the hard way in okay-watch,
a zero-byte object under the right key (blob-source-seam); the warning
lives on `produce`'s scaladoc, which is a comment, not a type. And a
row `Produce + G` can only be split by testing G's runtime class,
because a produced `String` is just a `String` (`Stream[[A] =>> A !
Produce + G, G]` says so: "G's operations must be class-distinct from
the element values"); `produced[A](e: Any): A` is the cast that stands
in for the answer type.

`Writer` had exactly this representation and left it in 12120c2a:
after five attempts to keep "the told value IS the operation" and
recover the answer type anyway — all five defeated by erasure, which
follows the representation (docs/existentials.md) — the sixth made it
a one-constructor GADT, `Say(w)`, and matching `Say(w)` refines the
answer to `Unit`. Cost, measured twice: an isolated build-and-fold of
10k tells put the wrapper at +25% (59.8 -> 75.0 us); the real
WriterBenchmark came in at 198.0 against 203.2 — no cost. The
accumulator dispatch in `Writer.fold` lost some of its worth (12%
rather than 35%, 95.9 against 108.4).

So the writer carrier `Unit ! Writer % W` (asynchronous: `Source[W] =
Unit ! (Writer % W + Async)`, okay-stream/src/main/scala/Source.scala)
is the same stream as `Producer[W]` with the element named in the type
and the answer free. This spec is about moving the library onto it —
MEASURED FIRST, because 12120c2a's number is Writer's own loop and not
`Chunks`' — and about what gets deleted when two carriers become one.

## Interface

Nothing new is added. What callers depend on after each stage:

```scala
// today: two carriers for one stream, bridged by hand
type Producer[A]  = A ! Produce                       // element = answer
type Chunks[A]    = Producer[Chunk[A]]                // okay-stream Chunks.scala
type Source[W]    = Unit ! (Writer % W + Async)       // element named, answer Unit
Source.fromProducer / ofProducer / toProducer         // the bridges (Source.scala ~132-162)
Producer.fold / each / concat / log                   // duplicates of Writer.fold / uncons / of

// stage 1: the rule, and the pure writer stream given a name
//   NEW STREAMING SEAMS ARE TYPED ON THE WRITER CARRIER.
//   (name to be settled in stage 1 — see Decisions; the shape is fixed)
type <PureSource>[W] = Unit ! Writer % W
// an outcome travels beside the elements, typed:
def get(key: String): Either[String, Unit] ! (Writer % Chunk[Byte] + Async)

// stage 2: what disappears, module by module
//   produced[A](e: Any): A                          — the cast
//   Producer.fold / each / concat                   — Writer's do the job
//   Stream[Producer, Pure], Stream[A ! Produce + G, G] — Writer's instances remain
//   Source.fromProducer / ofProducer / toProducer   — no second carrier to bridge
//   Producer itself: an alias of the writer stream, or the documented pure
//   special case if stage 0 shows the chunked hot path needs it
```

## Behavior

Stage 0 — measure (the decision gate; no source changes outside benchmarks):
- [ ] `Producer[Chunk[A]]` against `Unit ! Writer % Chunk[A]` on `Chunks.fold`
      and `Chunks.map` (okay-stream Chunks.scala ~162, ~283), same N, same
      chunk size, arms ALTERNATING in one JMH invocation
- [ ] the elementwise `Stream.fold` over both carriers (src/main/scala/Stream.scala)
- [ ] the bridges as they stand: `Source.fromProducer` and `toProducer` on a
      chunk producer, so their cost is known BEFORE they are deleted
- [ ] one byte lane through okay-blob (`Blob.get` -> `Producer.each` today)
- [ ] every row prints a COUNT beside the time (elements folded) and the
      host load; a row goes to src/jmh/history.tsv (date, sha, load,
      workload, mine, ref, ratio, note) — the performance skill's format
- [ ] the verdict is written into `## Results` below AND into the sprint
      item, with the numbers: "within noise" means the arms' difference
      is smaller than the spread between two identical runs in the SAME
      session; "a real loss" means the same sign across three alternating
      rounds and larger than that spread

Stage 1 — the rule (only if stage 0 says within noise, or loses only on `Chunks`):
- [ ] docs/guide.md states the rule: a new streaming seam names its element
      in the type — `Source[W]` when it performs Async, the pure writer
      stream otherwise; `Producer` is the pure special case / an alias
- [ ] the trap is closed by type: a test asserts with `compileErrors` that
      `pure(a)` does NOT type-check where an element is expected on the
      writer carrier, beside the existing runtime check that `produce`
      and `tell` do emit
- [ ] the pure writer stream has a name and the two Stream instances
      Writer already has (Writer.scala ~359, ~370) are the ones the name
      resolves to — no third pair

Stage 2 — migrate, one module per lane, `Chunks` LAST:
- [ ] okay-blob: `Blob.get` answers its outcome beside the bytes as
      `Either[String, Unit] ! (Writer % Chunk[Byte] + Async)`; `Producer.each`
      call sites (Backup, Offload, okay-watch restore) become a Writer fold
- [ ] okay-cluster Flow/Flows, okay-persist Streams/Wire, okay-sql/okay-jdbc,
      okay-docs and its backends, the kafka/fs2/zio/java interops — each
      lane: `sbt Test/compile` across the WHOLE repo first (a signature
      change; see memory signature-change-test-compile-first), then the gate
- [ ] `Chunks[A]` retyped; `Chunks.generate/range/fromIterator` emit with
      `tell`; the specialised `iterator` walk that `Stream[Producer, Pure]`
      has is carried over to the writer instance, not lost
- [ ] deletions land with the last module: `produced`, `Producer.fold/each/
      concat`, the Produce Stream instances, the three Source bridges
- [ ] `Put[Producer]` (from the put-de-diagonal lane) follows Producer: an
      alias keeps it for free, a deletion removes it with the type

## Out of scope

- `Put`'s signature and its instances — the put-de-diagonal lane
  (claimed 2026-09-19, 307304cb), which lands first and independently.
- `Channel`, `Drain`, the chunk buffers, `Flush`: the concurrent side
  is untouched; it consumes streams through `Stream` and does not care
  which carrier.
- The representation of `Chunk` (an `ArraySeq` alias in the core).
- A new unboxed indexed signature (`Emit[W]` with the raw value as the
  operation): rejected below, not deferred.

## Design

**The two carriers are the same coalgebra.** Both are observed by
`uncons`; `Writer.uncons` answers `Either[A, (W, rest)]` — the answer
kept at the end — where the Produce instance answers `Option` and
forgets it (Stream.scala's header calls Writer "the cousin with a
richer observation"). Every combinator lands in `LazyList` through
`toLazyList`, so consumers do not change; only the seams that
CONSTRUCT a stream and the walks that read the answer do.

**Why not fix Producer in place.** The fix would be a signature indexed
by the element with the answer separate, `Unit ! Emit[W]`, keeping the
raw value as the operation (no node). That is the identity encoding
Writer tried five times to keep; every attempt failed because the
answer type has to be recovered from a representation that carries no
tag, and erasure follows the representation. The thing that worked
was to carry the tag, and it measured free on the real benchmark. Not
re-arguing that here; re-MEASURING it on Chunks is stage 0.

**Where the cost could be.** A `Say` node per element is one small
allocation per element on top of the Bind the interpreter already
allocates. On the chunked path there is one element per CHUNK
(64-1024 values), so the per-element node is amortised away and the
expected difference is nil; on the elementwise `Stream.fold` path the
node is a fraction of the ~150 us per 10k the interpreter step costs
(Stream.fold's own comment). The one place a difference is plausible
is `Chunks.fold`'s accumulator dispatch, the analogue of the 12% that
`Writer.fold` lost — which is why stage 0 measures the fold and the map
separately rather than one pipeline.

**Order of migration.** Leaf modules first (blob, then cluster,
persist, sql/jdbc, docs, interops), `Chunks` last: it is the hot one,
it is the type five modules name, and its retyping is the moment the
bridges can go. Each module lane is independently shippable, and the
bridges keep working until the last one.

**What Writer already has that Producer duplicates.** `Writer.of`
(any Stream told into a writer program), `Writer.fold` (with the
accumulator dispatch), `Writer.collect`, `Writer.uncons` in both rows,
two Stream instances (pure and `+ G`), and — from the put-de-diagonal
lane — a `Put` instance. `Producer.log` (a printing Handler) has no
Writer twin and is not worth one; a `Writer.fold` with `println` is
the same thing.

## Decisions

- **Measure before migrating** — chosen because the only number in hand
  (12120c2a: 198.0 vs 203.2) is Writer's own loop, and Chunks is where
  the library's throughput claims are made (docs/benchmarks.md, the
  wroclaw lanes). Rejected: migrate on the strength of that number
  (adjacent numbers are camouflage; a cost claim must name the SHAPE it
  measured).
- **The writer carrier, not a new tagged-but-unboxed signature** —
  chosen because the tag is what made the answer type recoverable in
  the six-encoding series (docs/existentials.md) and it measured free.
  Rejected: `Emit[W]` over the raw value (the five failed encodings,
  again).
- **Chunks last, leaves first** — chosen so every lane before the last
  is a pure win in types with the bridges still standing. Rejected:
  retype `Chunks` first (every module breaks at once; one lane of ~60
  files cannot be gated in pieces).
- **If stage 0 shows a real loss on Chunks** — Producer stays as the
  chunked hot carrier, stage 1's RULE still applies to new seams, and
  the loss is recorded here with its numbers so nobody re-measures it
  from memory.
- **The name of the pure writer stream** — an operator preference
  (naming), settled in stage 1, not here. Candidates: keep spelling it
  `Unit ! Writer % W` (no alias; explicit, and what `Writer.of`
  already returns), or one short alias beside `Source`. A third
  carrier name is NOT a candidate.

## Results

Empty until stage 0 runs. Fill with the table (shape, N, chunk size,
mine, ref, ratio, host load, sha) and the verdict line the sprint item
repeats.
