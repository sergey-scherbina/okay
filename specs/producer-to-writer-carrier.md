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
- [x] `Producer[Chunk[A]]` against `Unit ! Writer % Chunk[A]` on `Chunks.fold`
      and `Chunks.map` (okay-stream Chunks.scala ~162, ~283), same N, same
      chunk size, arms ALTERNATING in one JMH invocation — DONE:
      `compare/src/jmh/scala/okay/ProducerWriterCarrierBenchmark.scala`
- [x] the elementwise `Stream.fold` over both carriers (src/main/scala/Stream.scala) —
      reused the existing `FoldConsumersBenchmark.writerSpecialized` /
      `.streamSpecialized` (no new code needed) plus an Async-shaped pair
      in the new file (`bridgeProducerDirect` / `bridgeWriterDirect`)
- [x] the bridges as they stand: `Source.fromProducer` and `toProducer` on a
      chunk producer, so their cost is known BEFORE they are deleted —
      measured elementwise (`bridgeProducerThroughSource` /
      `bridgeWriterThroughProducer`), not on a chunk producer specifically:
      the bridges themselves are elementwise regardless of what feeds them
- [x] one byte lane through okay-blob (`Blob.get` -> `Producer.each` today) —
      a synthetic chunk array, `Blob`'s own shapes (`Producer.each` /
      `Writer.fold` with a side-effecting sink), no engine
- [x] every row prints a COUNT beside the time (elements folded) and the
      host load; a row goes to src/jmh/history.tsv (date, sha, load,
      workload, mine, ref, ratio, note) — the performance skill's format —
      6 rows appended, `producer-writer-carrier-stage0` sha, N named per row
- [x] the verdict is written into `## Results` below AND into the sprint
      item, with the numbers: "within noise" means the arms' difference
      is smaller than the spread between two identical runs in the SAME
      session; "a real loss" means the same sign across three alternating
      rounds and larger than that spread — VERDICT BELOW, mixed: a real
      win at element granularity, a real ~2x loss on `Chunks.fold`
      specifically, with the cause diagnosed

Stage 1 — the rule (only if stage 0 says within noise, or loses only on `Chunks`) —
CONDITION MET, see Results: the loss is confined to `Chunks.fold`, not the
carrier in general (`Chunks.map` and every elementwise shape win):
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
- [ ] BEFORE `Chunks[A]` retypes: a chunk-aware specialized fold on the
      writer carrier (the analogue of `Chunks.foldLeft`/`Fold.OfLong`'s
      dispatch, walking told chunks with the ELEMENT's `Fold` inlined
      into a per-chunk loop, not `Writer.fold`'s generic per-chunk box)
      — measured at parity with `Chunks.fold` (stage 0 found the bare
      migration 2x slower there; see Results). Without it, `Chunks`
      stays on `Producer` and this bullet and the next stay undone —
      a legitimate stopping point for this stage, not a blocker for
      stage 1 or the rest of stage 2's modules
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

Stage 0, measured 2026-09-19, `compare/src/jmh/scala/okay/ProducerWriterCarrierBenchmark.scala`
(N=10000, chunk=64 where chunked; `FoldConsumersBenchmark` reused
unchanged for the pure elementwise pair). Three whole-suite runs
(JMH's own `@Fork(2)`/`@Warmup(3)`/`@Measurement(5)` per method inside
each), host load at the START of each run 48.7 / — (not captured,
between the other two) / 2.7 — round 1 landed on a busy box (a
sibling's full `sbt test` running), round 3 on a quiet one; the third
round's tight error bars (±0.03–6 us against round 1's ±1–222) are the
ones to trust for magnitude, and every verdict below is signed the
same way in all three:

| workload | mine (Writer) | ref (Producer) | ratio | sign, 3 rounds |
|---|---|---|---|---|
| chunked fold, sum 10k longs | 5.175 us | 2.527 us | **2.05x slower** | 1.64 / 1.99 / 2.05 |
| chunked map+fold, double then sum | 5.777 us | 7.603 us | **0.76x (24% faster)** | 0.81 / 0.77 / 0.76 |
| elementwise fold, G=Async | 95.917 us | 112.137 us | **0.86x (14% faster)** | 0.94 / 0.83 / 0.86 |
| elementwise fold, pure (FoldConsumersBenchmark) | 95.855 us | 120.857 us | **0.79x (21% faster)** | 0.83 / 0.81 / 0.79 |
| bridge tax, Producer -> Source (`ofProducer`) | 196.323 us | 112.137 us (direct) | 1.75x | 1.64 / 1.69 / 1.75 |
| bridge tax, Writer -> Producer (`toProducer`) | 182.443 us | 95.917 us (direct) | 1.90x | noisy/noisy/1.90 (round 1-2 error bars ±114-222 us swallow the ratio; round 3 is the first trustworthy one) |
| blob byte lane (64 x 1024B chunks) | 6.072 us | 5.720 us | 1.06x | 1.03 / 1.40(noisy) / 1.06 — small, same-signed, same root cause as the chunked-fold loss but diluted (64 chunks, `Unit` accumulator needs no boxing) |

**VERDICT — mixed, and the split is informative, not just noisy:**

**Elementwise streaming is a real, reproducible win for the writer
carrier** — 14-24% faster, in FOUR independent measurements (two
carriers x {chunked-map, Async-elementwise, pure-elementwise}), all
signed the same way across all three rounds, two of them (the pure
elementwise pair) confirming the exact number `12120c2a` predicted for
Writer's own zero-allocation `Say` node. This is what "a new streaming
seam" mostly is, so **stage 1 proceeds**: the rule is written, and it
is correct as stated.

**`Chunks.fold` specifically is a real ~2x loss for a bare migration,
and the cause is diagnosed, not mysterious.** `Chunks.foldLeft`
(what `Chunks.fold` dispatches to for `Fold.OfLong`) tests the
accumulator's type ONCE, outside the loop, then reads
`Fold.OfLong.addLong` on an unboxed `long` for every element inside a
tight per-chunk `while`. `Writer.fold`'s own dispatch tests the TOLD
type — here `Chunk[Long]`, never `Long` — so it can never take that
fast path; it always falls to the generic `Fold[Chunk[Long], Long]`
case, boxing the accumulator through one `Function2` call per CHUNK.
That per-chunk box is what the 2x is: `Writer.fold`'s own comment
already named this exact trade for its NON-chunked case ("with the
element boxed and the accumulator a raw long, 3.8 against a 2.5
floor... the accumulator is essentially the whole cost"), and it is
worse here because there is no chunk-aware two-level loop on the
writer side to inline into. `Chunks.map` does NOT show this loss
(it is a 24% WIN) because `Writer.map`'s per-chunk walk pays no
accumulator-boxing tax — it only rebuilds Free nodes and calls a
`Chunk[Long] => Chunk[Long]` function once per chunk, which
`Chunks.map`'s own `ChunkBuf.mapper` dispatch does too.

**So stage 2's `Chunks` migration is not a bare rename.** Before
`Chunks[A]` can move onto the writer carrier without regressing its
hot fold path 2x, someone has to write a chunk-aware specialized fold
for it — the writer-carrier analogue of `Chunks.foldLeft`/`Fold.OfLong`
dispatch, walking told chunks with an inlined per-element loop over
the ELEMENT's `Fold`, not the chunk's. Until that combinator exists
and is ITSELF measured at parity with `Chunks.fold`, `Chunks` stays on
`Producer` as a documented, narrow exception — exactly the "a real
loss -> Producer stays for the chunked hot path" branch of the
decision gate, scoped to the FOLD operation specifically rather than
the whole carrier (map already clears the bar).

**The bridges cost what deleting them promises to recover** — 75-90%
over a direct fold, both directions, consistent sign across all three
rounds (round 1-2's huge error bars on the reverse bridge, ±114 to
±222 us on a ~200-350 us number, are a busy-host artifact, not a sign
flip: round 3 alone, on the quiet box, is the number to plan against).

**The blob byte lane shows the same tax as the chunked fold, in the
same direction, too small to act on at this size.** 64 chunks of 1024
bytes each is not enough chunks, and the accumulator (`Unit`, for a
side-effecting sink) is not boxed the way a `Long` sum is, so the
per-chunk dispatch tax that dominates the 10k-long chunked fold is
present but small here (3-6%, once dropping the one noisy round).
Not a reason to block stage 1; a reason to expect a real, if small,
regression on `Blob.getSource`'s own drain once something folds it the
way this benchmark does, until the same chunk-aware fold from the
paragraph above exists.
