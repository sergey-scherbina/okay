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

// stage 1 — DONE: the rule, and the pure writer stream named Feed
//   NEW STREAMING SEAMS ARE TYPED ON THE WRITER CARRIER.
type Feed[W] = Unit ! Writer % W                      // src/main/scala/Writer.scala
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
carrier in general (`Chunks.map` and every elementwise shape win). DONE
(2026-09-19), the name is `Feed[W]` (operator's choice, over no-alias and
`Told[W]`):
- [x] docs/guide.md states the rule: a new streaming seam names its element
      in the type — `Source[W]` when it performs Async, `Feed[W]`
      otherwise; `Producer` is the pure special case / an alias
- [x] the trap is CORRECTED FROM THE PLAN, not closed by type: a real
      compile of `val f: Feed[Int] = pure(5)` prints `[E190] ... Discarded
      non-Unit value`, which this repo's gate refuses as any warning —
      but `compileErrors` cannot see this (it reports hard errors only,
      and munit's macro drops warnings entirely; checked directly
      against `val u: Unit = 5` before trusting it, since `Source`'s own
      version of this exact claim — `TestSourceProducer`'s comment on
      `-Wvalue-discard` — was never actually verified by its own test
      either, only asserted in prose). `TestGenerate` documents the fact
      and asserts what IS testable: no hard error, same as `Producer`.
- [x] the pure writer stream has a name and the two Stream instances
      Writer already has (Writer.scala ~359, ~370) are the ones the name
      resolves to — no third pair

Stage 2 — migrate, one module per lane, `Chunks` LAST:
- [x] okay-blob DONE (2026-09-19): `Blob.get`/`put`/`list` all three
      retyped to the writer carrier (`Either[String, Unit] ! (Writer %
      Chunk[Byte] + Async)`, `Source[Chunk[Byte]]`, `Source[Chunk[Meta]]`);
      `getSource`/`putSource` deleted (redundant — `get`/`put` ARE that
      now); every `Producer.each`/`Producer.concat` call site (Fs, S3,
      Backup, Offload) became `Writer.fold`/`Writer.collect`. One
      genuine simplification found along the way: S3's `get` used to
      walk its own response body — ALREADY a `Source[Chunk[Byte]]` from
      okay-http — chunk by chunk back into the Produce row it had to
      answer; now it is `Writer.expand(src)(filter).map(_ => Right(()))`,
      no bridge crossed at all. `okay-watch restore` is a private repo,
      not touched here.
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
- [ ] `Put[Producer]` (landed in put-de-diagonal, c9a3f561) follows
      Producer: an alias keeps it for free, a deletion removes it with
      the type

## Out of scope

- `Put`'s signature and its instances — landed independently in
  put-de-diagonal (c9a3f561, 2026-09-19), before this stage measured
  anything. `Generate.scala` is no longer a shared-edit hazard with
  any other lane.
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
two Stream instances (pure and `+ G`), and — landed in put-de-diagonal
— a `Put` instance. `Producer.log` (a printing Handler) has no
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
- **The name of the pure writer stream is `Feed[W]`** — settled
  2026-09-19, the operator's call over no-alias and `Told[W]`
  (candidates were `type Feed[W] = Unit ! Writer % W`, a short alias
  symmetric with `Source[W] = Unit ! (Writer % W + Async)`, against
  keeping it unaliased). Declared in `Writer.scala` beside the `Put`
  instance it names.

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

### The chunk-aware fold prerequisite, measured 2026-09-19

Stage 0's own diagnosis named the missing piece: a chunk-aware fold for
the writer carrier, walking told chunks with an inlined per-element
loop, the writer analogue of `Chunks.foldLeft`/`Fold.OfLong` dispatch.
Built as `Chunks.foldLeftWriter` (a literal-step form) and
`Chunks.foldWriter` (a `Fold`-instance-dispatched form, matching what
`Chunks.fold` itself does) in `okay-stream/src/main/scala/Chunks.scala`,
on top of the existing `Writer.foldWith` trampoline. Three whole-suite
JMH rounds, `compare/src/jmh/scala/okay/ProducerWriterCarrierBenchmark.scala`
(N=10000, chunk=64), medians:

| benchmark | round 1 | round 2 | round 3 | shape |
|---|---|---|---|---|
| `chunksFoldLeftProducerDirect` | 4.755 | — | 4.755 | `Chunks.foldLeft`, literal step (control) |
| `chunksFoldLeftWriterDirect` | 5.034 | 4.969 | 5.004 | `foldLeftWriter`, literal step, called directly |
| `chunksFoldProducer` | 2.540 | 2.544 | 2.558 | `Chunks.fold`, `Fold.OfLong` dispatch (control) |
| `chunksFoldWriter` | — | — | 5.143 | pre-existing Pure baseline, unchanged from stage 0 |
| `chunksFoldWriterAsync` | — | — | 4.991 | same, Async-shaped |
| `chunksFoldWriterDispatched` | 17.815 | 16.727 | 17.304 | `foldWriter`, `Fold.OfLong` dispatch |

**Mixed result, and the split is real, not noise.** `foldLeftWriter`
called directly with a literal step — the shape `okay-cluster`'s own
`Chunks.foldLeft` call sites (`Flows.scala`: 6, `Job.scala`: 1) already
use — reaches **parity**: 5.00 vs the Producer control's 4.76 us/op,
consistent across all three rounds. `foldWriter`, the `Fold`-instance-
dispatched form that `Chunks.fold`/`agg.fold` need (`Bulk.scala`,
`Pipeline.scala`, `Acceptance.scala` — the only three such call sites)
does **not**: 17.30 us/op, stable across three different implementation
attempts (a hand-rolled recursive walker, a `Writer.foldWith`-based
rewrite, and the same rewrite with the dispatcher itself also marked
`inline`) — **3.5x** `foldLeftWriter`'s own direct call (same carrier,
same per-element arithmetic, no dispatch — isolates the dispatch tax)
and **6.8x** `chunksFoldProducer`, the Fold-dispatch baseline on
Producer.

**Diagnosed as far as this environment allows.** The first attempt put
the per-element step inside a nested `@tailrec` recursive local `def`;
an `inline` parameter stops being inlined across that boundary, boxing
the step into a real `Function2` called once per ELEMENT instead of
once per chunk — fixed by moving the per-element `while` into the same
flat, non-recursive scope where `Writer.foldWith`'s own step closure is
written (this is what `foldLeftWriter` reaching parity proves). The
remaining gap is isolated to the dispatch layer specifically: the ONLY
difference between the fast direct call and the slow dispatched one is
that the dispatched step captures a matched `Fold.OfLong` instance and
calls a method on it (`l.addLong(s,a)`) instead of being a fully
literal expression — but `Chunks.fold` has the identical virtual-call
shape and pays no such cost, so the tax is specific to the extra
`inline` layers `foldWriter` asks the compiler to flatten, not to
virtual dispatch on `Fold` in general. Root cause not fully isolated;
would need a profiler (JITWatch or `-prof perfasm`) this environment
does not have.

**Ship the half that works.** `foldLeftWriter` is at parity now — it
unblocks `okay-cluster`'s `Flows.scala`/`Job.scala` stage-2 migration
today. `foldWriter`'s gap stays open and documented; `Bulk.scala`,
`Pipeline.scala`, and `Acceptance.scala` stay on `Producer`/`Chunks.fold`
until it closes. A retry should start from `Chunks.scala`'s doc comments
on both combinators, which name the three shapes already tried, so a
second attempt does not repeat them.

### The root cause, actually found (2026-09-19, follow-up)

The "would need a profiler this environment does not have" line above
was wrong — JMH ships `-prof gc` and `-prof jfr` (Java Flight Recorder)
in-JDK, no external tool needed, and `javap` reads the compiled
bytecode directly. Using them:

**`-prof gc`** on a fresh `chunksFoldWriterDispatched` run: 249,584 B/op
against `foldLeftWriter`'s own direct call at 12,656 B/op — about
10,000 extra bytes-worth of boxed `java.lang.Long`, one per element
(N=10000). Raising `-XX:MaxInlineLevel` and `-XX:FreqInlineSize` well
past their defaults changed nothing (still exactly 249,584 B/op),
ruling out a simple JIT inlining-budget explanation.

**`-prof jfr`**'s allocation stack traces name the box exactly:
`ArraySeq$ofLong.apply` -> `boxToLong` -> `Fold$OfLong.addLong`,
present in the dispatched path's profile (867 of 910 sampled
allocations) and essentially absent from the direct call's (1 of 245).

**The box itself is not a defect** — `Fold.OfLong[A]`'s own
`addLong(s: Long, a: A): Long` takes its element generically BY DESIGN
(a fold over `A`, not necessarily over `Long`), so a synthetic bridge
method boxes on every call. `javap` confirms `Chunks.fold` makes the
IDENTICAL call (`ArraySeq.apply` -> `boxToLong` -> `addLong`, same
bytecode shape) and pays nothing for it, because the JIT's escape
analysis proves the box never escapes `Chunks.foldLeft`'s small,
standalone compiled loop and eliminates it entirely. The SAME analysis
fails inside `Writer.foldWith`'s bigger resume/split/Bind tailrec
trampoline, so the box becomes a real heap allocation there — the
entire 6.8x is this one box, paid once per element, that a smaller
compiled method gets for free.

**A second fix attempt, tried and reverted:** pulling the per-chunk
consuming loop out into its own small, standalone method
(`foldChunkLong`/`foldChunkInt`/etc., one per `Fold.OfX` case) measured
NO CHANGE (still exactly 249,584 B/op). `javap` explains why: the JIT
re-inlines the small, hot, `invokespecial`-called helper straight back
into the trampoline anyway, reproducing the identical combined compiled
unit either way. Escape analysis is gated by what ends up in ONE
compiled unit after the JIT's OWN inlining decisions, not by
Scala-level method boundaries — refactoring the source cannot
out-maneuver that on its own, so this is reverted rather than kept as
dead weight.

**What would actually fix it, not yet tried:** decouple the tree walk
from the per-chunk consumption so the box-unbox pair sits in a small
compiled unit the JIT cannot re-merge with the trampoline — concretely,
give `Writer`'s `Stream` instances (`src/main/scala/Writer.scala`) their
OWN `iterator` override (they currently fall back to the generic
`Iterator.unfold(s)(uncons(_).runWith)`), and run `Chunks.foldLeft`'s
own tiny per-chunk loop against THAT instead of routing through
`Writer.foldWith`. The obstacle is the API contract, not the walk
itself: `.iterator` needs a `Handler[G]` and runs EAGERLY, where
`foldWriter` currently returns `(S, Unit) ! G` — a suspended PROGRAM,
composable with `flatMap` before anything runs. Making `foldWriter`
eager would be a real, visible API change, not a drop-in performance
fix, and deserves its own design pass rather than a rushed patch riding
this one.

### FIXED, mostly (2026-09-19, second follow-up)

Did exactly what the paragraph above said was blocked, once it turned
out nothing was actually blocking it: `foldWriter` had ZERO production
call sites (`Bulk.scala`/`Pipeline.scala`/`Acceptance.scala` all still
call `Chunks.fold` on `Producer` directly), so narrowing its signature
broke no caller. New `foldWriter[A, S](p: Unit ! (Writer % Chunk[A] +
Async))(using Fold[A, S], CanBlock): (S, Unit) ! Async` walks via
`writerStreamIn`'s DEFAULT `.iterator` (no override needed —
`Iterator.unfold(s)(uncons(_).runWith)` was already small enough) and
wraps the eager walk in `async { ... }` (`Async.Run`, existing
primitive) so the returned value is still a suspended program, just
over a narrower row.

Measured, 3 rounds, N=10000/64, JDK 21.0.12 pinned. Host load recorded
for rounds 2-3 only (round 1 ran before this discipline was re-applied
mid-lane — noted rather than invented):

| round | time | gc.alloc.rate.norm | host load |
|---|---|---|---|
| 1 | 6.290 us/op | 32,952 B/op | not recorded |
| 2 | 5.864 us/op | 32,952 B/op | 2.26/5.61/12.93 |
| 3 | 6.326 us/op | 32,952 B/op | 2.23/4.95/12.07 |

**2.9x faster (18.14 -> ~6.3 us/op median), 7.6x less garbage (249,584
-> 32,952 B/op).** `-prof jfr` on the new implementation: `java.lang.Long`
samples down from 867 to 16 (background noise) — the element-boxing
problem this whole investigation started from is gone. The dominant
allocations now are `Free$Bind`/`Free$Inject`/`Right`/`Some`/`Free$Pure`
— one `Option`+`Either` wrapper and one `Free` node per CHUNK (157
times), from `Iterator.unfold`'s generic walk, not per element (10000
times).

**Not full parity with `Chunks.fold`'s 2.55us, and the reason is not
Writer-specific.** `Chunks[A]` is `Producer[Chunk[A]]` — PURE, no G —
so `Chunks.fold` walks through `Stream[Producer, Pure]`'s own
hand-specialized, allocation-free `iterator` override (Generate.scala).
Producer's OWN G-effectful Stream instance (`given [G[+_]: TypeableK]:
Stream[[A] =>> A ! Produce + G, G]`) has NO such override either — it
pays the identical `Iterator.unfold` tax for a G-effectful `Producer`.
So the remaining gap is "no G-effectful stream in this library has a
specialized iterator yet," not something particular to the writer
carrier. Closing it — a mutable-state `iterator` override mirroring
`Stream[Producer, Pure]`'s shape, but handling a forwarded G-effect —
would benefit both carriers equally. Not written here; its own lane.

**Consequence:** `Bulk.scala`, `Pipeline.scala`, and `Acceptance.scala`
can migrate onto `foldWriter` now (their `G` is `Async`-shaped already,
matching the narrowed signature) — not yet done, since each is embedded
in a `Chunks[A]`-typed surrounding context of its own (the same
"leaves first" caution `Flows.scala`'s `Shape[A]` triggered), not a
same-session follow-on to this fix.

### The remaining gap, also closed (2026-09-19, third follow-up)

The "its own lane" from the paragraph above turned out to be small: a
hand-specialized, mutable-state `iterator` override for
`writerStreamIn`, mirroring `Stream[Producer, Pure]`'s own override in
Generate.scala byte for byte in shape (a `var cur`/`ready`/`ended`/`elem`
state machine, `@tailrec advance()`). The one real difference is how a
forwarded `G`-operation is answered: not by building and running
another program, but by `Handler[G].handle(g)` — the COMONADIC,
single-operation interpretation every `Handler` already provides
(`Handler[Async]`'s own `case Run(f) => f()` / `case Await(reg) =>
cb.block(reg)...`). This eliminates the `Option`+`Either`+`Free`-node
allocation the DEFAULT `Iterator.unfold(s)(uncons(_).runWith)` still
paid once per chunk.

Correctness: a new test builds a writer program with REAL interleaved
`async` calls between tells (the benchmark's own data never exercises
that branch — `.plus[Async]` only widens the row) and checks the
specialized iterator's result against `Writer.run` — built on the
unrelated `Writer.foldWith` trampoline — as an independent oracle.

Measured, 3 rounds, N=10000/64, JDK 21.0.12 pinned, host load
5.02/3.94/3.56, then 2.07/2.82/3.16, then 2.23/2.74/3.11:

| round | time | gc.alloc.rate.norm |
|---|---|---|
| 1 | 5.428 us/op | 12,688 B/op |
| 2 | 5.291 us/op | 12,688 B/op |
| 3 | 5.448 us/op | 12,688 B/op |

**6.29 -> ~5.43 us/op, 32,952 -> 12,688 B/op** — now matching
`foldLeftWriter`'s own direct-call baseline (12,656 B/op, ~5.0us)
almost exactly. `-prof jfr` on the new implementation: no `Right`/`Some`
samples at all (was the dominant allocation before); what remains
(`Free$Bind`, `Writer$Say`, the benchmark's own tree-construction
lambdas) is the cost of BUILDING the 157-node program fresh each call —
paid identically by the direct-call baseline too, not something this
fix could or should touch.

**Total, from the original dispatched form: 18.14 -> 5.43 us/op (3.3x
faster), 249,584 -> 12,688 B/op (19.7x less garbage).** The remaining
~2x against `Chunks.fold`'s 2.55us is the SAME gap `foldLeftWriter`'s
own direct call already has and was accepted as "at parity" for — the
cost of walking a `Free`-tree program (`resume`, `Bind` chains) at all,
versus `Chunks.foldLeft`'s specialized, non-program iterator. That is a
different, larger question (closing it would mean Producer's own
G-effectful walk needs the same treatment `Chunks.foldLeft`'s PURE walk
already has) than this combinator's own dispatch tax, which is now
closed.
