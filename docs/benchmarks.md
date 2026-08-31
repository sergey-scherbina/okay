# Benchmarks: the cases, the numbers, and the why

Every number here is JMH average time (us/op, lower is better) from
`src/jmh/history.tsv`, which records every run with its date, commit,
host load and protocol — including the experiments that were tried
and REFUTED, so nobody re-runs them blind. The working rule of
specs/interpreter-optimization.md applies throughout: one measurement
is a hypothesis, not a result; lanes quoted together were measured in
the same session on the same (busy, honestly noted) host.

Versions compared: cats-effect 3.5.7, ZIO 2.1.14, kyo 0.16.2,
atnos-eff 7.0.4, fs2 3.10.2, circe 0.14.10.

Run them yourself: `sbt 'Jmh/run .*Fib.*'` (core lanes),
`sbt 'compare/Jmh/run .*Compare.*'` (ecosystem lanes; the heavy
dependencies live only in the compare module),
`sbt 'compare/Jmh/run RagBenchmark'` (retrieval),
`sbt 'compare/Jmh/run EmbeddingBenchmark'` (the embedding representation).

One claim here is not a time at all — structural chunking's advantage
is that a chunk is a WHOLE definition, so it is measured as a
percentage by `okay-rag`'s `TestChunkQuality` and reported in §11
beside the microseconds it costs.

---

## 1. Bind chain — 10k left-nested flatMaps, built and run

| **okay Eager** | kyo | **okay Cont** | **okay Free** | cats Free | cats Eval | cats IO | ZIO | atnos |
|---|---|---|---|---|---|---|---|---|
| **5.1** | 58 | **89** | **95** | 129 | 136 | 153 | 181 | 260 |

**What it measures.** The raw cost of the monadic plumbing itself:
build a 10 000-step flatMap chain by foldLeft (the WORST, left-nested
shape), then run it. No effects, no I/O — just who pays how much per
bind.

**Why okay's numbers.** Three encodings, one interface, chosen per
program:

- `Free` (the tree) stays fast because `fold` REBALANCES left-nested
  binds with tail-recursive rotations — the "reflection without
  remorse" problem solved with two lines of pattern match instead of
  a type-aligned queue (measured: stepping one-by-one costs only ~8%
  over bulk, so the queue is unneeded, with evidence).
- `Cont` is the same discipline one level down: a defunctionalized
  continuation monad whose runner is one tail-recursive loop, plus
  closure FUSION up to a depth budget (flatMap/map merge into the
  Shift closure while shallow, spill to Bind after — kept after
  measuring −10..28% across generator lanes).
- `Eager` is the kyo trick as an OPT-IN encoding (`import
  Eager.given`): pure binds apply at construction, so "running" the
  chain is running nothing. 10x under kyo on this lane — with kyo's
  hazards STATED, not hidden (see the asterisk below).

**Why the competitors' numbers.** cats Free lacks the rotation (its
fold re-associates by allocation); cats IO and ZIO run every bind
through a fiber runtime — shift checks, trace buffers, interruption
machinery — pure overhead when nothing suspends; atnos-eff pays the
open-union tagging of classic freer on every operation.

**The asterisk that reframes the whole table.** kyo's 58 is
FRONT-LOADED: construction evaluates. Build-only lanes
(LazinessBenchmark): okay 13.8 (15% of its full cost), cats IO 26.9,
kyo 58.5 — **101%**: for kyo, construction IS the computation.
Building an infinite pure-recursive program runs 513 iterations
uninvited (their safepoint budget); an exception in a flatMap lambda
throws at BUILD time; an effect runs once-at-build, so the value is
not reusable as a description (all three demonstrated in
compare/TestLaziness). In build-many-run-few scenarios okay is 4.2x
cheaper; and okay offers the same trade EXPLICITLY as `Eager`,
per-program, instead of as the only semantics.

## 2. Reader — 10k asks · Writer — 10k tells

| Reader | **okay** | ZIO | cats Kleisli | atnos | kyo Env |
|---|---|---|---|---|---|
| | **110** | 240 | 350 | 1737 | 362 756* |

| Writer | **okay** | cats WriterT/Chain | atnos | kyo Emit |
|---|---|---|---|---|
| | **286** | 1127 | 3202 | 386 322* |

**What they measure.** Handled operations at volume — the everyday
shape of effectful code.

**Why okay's numbers.** Reader runs at RELAY speed: a
tail-resumptive handler must resume exactly once, so handling is one
tail-recursive loop — no continuation capture, no allocation per ask
(relay measured 1.45x over the general handler on forwarding-heavy
work). Writer's `tell` is ZERO allocation: the operation is an
opaque IDENTITY signature — telling w IS the value w, no wrapper
node; the handler is a bespoke tail loop into a Vector.

**Why the competitors' numbers.** cats WriterT allocates a
tuple-in-monad per tell; Chain helps, the wrapping doesn't. atnos
pays union tagging again. The starred kyo numbers are ~1000x off
because kyo's Env/Emit resumption RE-TRAVERSES the pending
computation — quadratic on left-nested chains with handled ops. That
is precisely the pathology okay's Bind rotation exists to prevent;
the lane is the rotation's value made visible.

## 3. Choice — 2^13 branches, all collected

| List (floor) | **okay** | kyo | atnos |
|---|---|---|---|
| 580 | **1603** | 3834 | 5392 |

The one handler that is genuinely MULTI-SHOT: the continuation runs
once per alternative. okay's `runChoice` folds `k(x)` over the
alternatives on the Free tree directly; 2.8x from the bare-List
floor is the price of programs-as-values here, and it still halves
kyo. (Multi-shot is where one-shot-optimized runtimes can't follow:
this handler cannot be expressed with relay OR exceptions.)

## 4. Fork/join — 100 trivial fibers

| raw Loom (floor) | kyo | **okay** | ZIO | cats IO |
|---|---|---|---|---|
| 21 | 25 | **29** | 50 | 140 |

**Why okay's number.** There is almost no okay here — that is the
design. A fiber IS a virtual thread; spawn is `Thread.startVirtualThread`,
join parks. No fiber runtime of our own means nothing added over the
floor but 8us of bookkeeping. ZIO and cats IO pay their own
schedulers, run-loops and interruption protocols; kyo sits close to
the metal too (its scheduler is excellent) — we simply refuse to
compete by NOT having one.

## 5. Stream pipeline — map/filter/take(1000)/sum

| Iterator (floor) | **Staged** | **okay chunked** | **okay elements** | okay iterator | okay LazyList | kyo | ZIO | fs2 |
|---|---|---|---|---|---|---|---|---|
| 14 | **1.6*** | **16.9** | **23.6** | 53 | 143 | 239 | 692 | 1410 |

(*Staged and its same-run floor of 19.3 come from a different
session; the rest is one session. The 12x-under-the-floor number is
real: see below.)

**What it measures.** The bread-and-butter stream pipeline in each
library's fastest mode.

**Why okay's numbers, mode by mode.** This table is one design
principle at four price points:

- `toLazyList` (143): the memoized, re-observable bridge — you pay
  for the caching.
- `.iterator` (53): linear, fused, consume-once — a specialized
  tree-walk with a mutable cursor, no Option/tuple per element
  (measured −44% over the generic unfold when introduced).
- Chunks (23.6 / 16.9): the tree steps once per CHUNK; an element
  costs an array index. Transformers are chunk-in/chunk-out array
  passes; what remains over the Iterator floor is the Free-node
  stepping per chunk — the price of programs-as-values, amortized 64
  ways.
- `Staged` (1.6): when the pipeline's shape is known where it is
  written, inline combinators beta-reduce the WHOLE pipeline into
  one while-loop with every lambda inlined — no operator dispatch,
  no iterator protocol, and `take(1000)` exits by a plain boolean.
  UNDER the Iterator floor because Iterator itself pays virtual
  `hasNext`/`next` per element and the fused loop pays nothing.

**Why the competitors' numbers.** kyo, ZIO and fs2 all run
STREAMING RUNTIMES here: every element (or worst-case singleton
chunk) crosses effect-dispatch machinery. fs2's pull-model
Pull/Chunk plumbing is built for concurrency and resource safety,
priced per element; ZStream similarly. It's an honest worst-case for
them (singleton-unfold generators) and honestly noted as such.

## 6. Merge — two 500-element streams by readiness

| **okay chunked** | ZIO | okay elementwise | fs2 |
|---|---|---|---|
| **14.7** | 47 | 158 | 9031 |

Readiness-merge is what zip and ++ cannot express: a fiber per
source feeds one channel, the loser of every race simply arrives
later. Chunking the STREAM (not the queue — a chunked-queue variant
was tried and REFUTED, it's in history.tsv) beat ZIO's own
chunk-aware merge 3.2x. fs2's number is its worst case (singleton
elements through its concurrency machinery), stated as such.

## 7. Resource — 1000 bracketed acquire/use/release

| **okay region** | **okay bracket** | ZIO | cats IO | kyo |
|---|---|---|---|---|
| **18.7** | **26.3** | 106 | 197 | 8566 |

The region is a while-loop with a finalizer list — visible in the
number. The catch must see the CURRENT finalizer list (a tailrec
parameter would hide it — was a real bug, now a comment); releases
run in reverse at Pure or exception. Nothing suspends, so runtimes
built around suspension pay their machinery for nothing.

## 8. Generators — the 1000th Fibonacci, element by element

| Iterator | LazyList | **okay Producer** | okay LazyList | kyo | ZStream | fs2 |
|---|---|---|---|---|---|---|
| 12 | 13.5 | **18.4** | 35 | 61 | 172 | 245 |

Per-element unfold — the generator's honest per-element price. The
okay Producer is 1.5x from the bare-iterator floor; the streaming
libraries pay 10-20x in their per-element mode (their strength is
batches; so is ours — see lane 5).

## 9. Async terminals — runWith vs runAsync (10k ops)

| **runWith** (parking handler) | **runAsync** (the universal drive) |
|---|---|
| **241** | **289** |

The event-loop drive JS runs on, measured on the JVM against the
parking handler: **+20%**, and that is the whole price of
universality. The drive adds one atomic exchange per `Await`
(the callback may fire during registration, on any thread — whoever
loses the exchange continues) and NOTHING per `Run`. So the same
program is portable to a platform with no threads for a fifth more,
and on the JVM you simply keep `runWith`.

## 10. The text stack — lex, parse, reparse, codecs

Measured at load 2.4 with tight bars; 2.5KB JSON document, 50 members.

**Lexing** — and the one result that went the other way:

| element-wise | chunked (512) | chunked (64) | chunked (8) |
|---|---|---|---|
| **42.6** | 52.6 | 55.6 | 70.5 |

Chunked lexing is SLOWER, and the first explanation for it was
WRONG — which is worth more than the number. The three-size probe
showed per-chunk overhead falling away by size 512 while 23%
remained, and the conclusion drawn here was that the residual must be
per-CHARACTER boxing (`Chunk[A]` is an `ArraySeq` over
`Array[AnyRef]`, where the element-wise path reads `charAt`). Two
targeted experiments say otherwise: unboxed storage
(`Chunks.ofChars`, a primitive `Array[Char]`) bought 5%, and reading
that array directly instead of through the generic `apply` bought
another 3%. Eight percent, where the gap was twenty-three.

So the residual is per-CHUNK bookkeeping, not per-character work: a
`Vector.newBuilder`, a token-chunk allocation and a Free node for
each of the forty input chunks, against one builder and no chunk
machinery on the element-wise path. Both improvements are kept —
they are real, if small — and the chunked path's value remains what
it always was: streaming and constant memory over a source you
cannot materialize (a socket, a gigabyte file), where `Scan.all`
needs the whole input in memory.

**Parsing, full vs incremental:**

| full parse | incremental reparse (one-member edit) |
|---|---|
| 85.1 | **38.0** |

2.2x under the full parse for a one-in-fifty edit — real, and
honestly below what O(damage) suggests: the relex dominates the
reparse, and the common-prefix/suffix token scans are O(tokens).
That is the next lever if incremental parsing gets a demanding
workload; the correctness property (untouched subtrees returned BY
REFERENCE) is what the layer exists for.

**Codecs — where the contract shows up as a number:**

| | write | read |
|---|---|---|
| **okay CBOR** | **0.418** | **0.807** |
| circe (JSON) | 0.422 | 0.623 |
| **okay JSON** | **0.628** | **10.3** |

Read this table as a price list for CONTRACTS. Our CBOR write ties
circe's JSON write; our CBOR read is 1.3x it — that is the Schema
fold on its own, right next to a hand-tuned parser. Our JSON write
is 1.5x. But our JSON read is 16x slower, and that gap is the whole
point: `Json.read` runs chars → total scanner → total driver →
LOSSLESS CST → projection → Schema fold, where circe parses straight
into its AST. What the 16x buys is damage-as-data, byte-for-byte
losslessness, and a HALF-ARRIVED document that still decodes (the
LLM case). Need raw JSON decode speed and none of that? Use circe —
and keep the same `Schema` for the wire where the contract matters.

**BPE**: 300us for a ~3.3KB corpus. The rank scan is quadratic per
word — fine for v1, and the obvious lever the day tokenization gets
hot.

## 11. Retrieval — indexing, re-indexing, chunking, query

Measured at load 4.3–6.9 with tight bars (±2% or better on every lane
but one). The document is 8.5KB of Scala — 30 definitions with doc
comments, strings and nesting — and its 6.3KB Python twin.

**Indexing** — parse a file and build its symbol index:

| Scala, 8.5KB | Python, 6.3KB |
|---|---|
| **644** | **449** |

That is 13.2 and 14.0 MB/s warm. Cold, over a real tree —
`IndexReport` on this repository, 201 files and 898KB, including file
I/O and with no JIT warmup at all — the same work runs at 1.2 MB/s
and finishes in 744ms. Quote whichever matches your question; the
gap between them is warmup and I/O, not algorithm.

**Re-indexing after an edit** — one character changed in the 8.5KB
file:

| full re-parse | incremental reparse |
|---|---|
| 400 | **110** |

3.6x, and the same honest caveat as the JSON lane: below what
O(damage) alone suggests, because the relex dominates and the
prefix/suffix token scans are O(tokens). It is the ratio that makes a
live index of a repository the agent is EDITING affordable, which is
the whole reason this layer exists.

**Chunking — the price of parsing, and what it buys:**

| structural (parsed) | windows (unparsed) |
|---|---|
| 684 | **309** |

Structural chunking costs **2.2x** a sliding window. This is the one
table here where the slower number is ours on purpose, so the
comparison has to be made on the thing that actually matters — and it
is measurable, so `TestChunkQuality` measures it rather than asserting
it in prose. On the same file, at chunk counts deliberately matched
(12 structural against 11 windows):

| | definitions returned WHOLE |
|---|---|
| **structural** | **24 / 24 (100%)** |
| windows | 17 / 24 (71%) |

Nearly a third of the window chunks are half of one definition glued
to half of another. And the window split here is not a straw man — it
is `Split.windows`, exact, landing on the lexer's own token spans, and
it reassembles its source byte-for-byte at `overlap = 0`. The 2.2x
buys the 29%, and it is paid once at ingestion rather than per query.

**Per query, with no embedding service in play:**

| symbols (exact) | keyword (BM25) | hybrid (fused) | hybrid + assemble | vectors (240 segs, 1536 dim) |
|---|---|---|---|---|
| **0.55** | 12.4 | 17.9 | 16.7 | 347 |

Half a microsecond for an exact symbol lookup is the number worth
staring at: it is the argument for having a half of retrieval that
needs no vectors at all. "The definition of X" costs a map lookup and
a substring, so an agent can afford to ask it speculatively — which
is exactly what `Grounded.context` does on every turn.

**The embedding representation — the boxing question, asked again
and answered differently.** `Embedding` was `Vector[Float]`, and
`Vector` is a generic trie over `Array[AnyRef]`, so a 1536-component
provider vector was 1536 boxed `java.lang.Float` objects. Four ways
of holding the same numbers, one cosine at provider dimension:

| `Vector[Float]` | `ArraySeq[Float]` | `ArraySeq.ofFloat` | `Array[Float]` |
|---|---|---|---|
| 11.70 | **1.043** | **1.035** | **1.034** |

**11.3x**, and the three unboxed forms are indistinguishable — the
JIT devirtualizes the generic `apply`, so the win needed only a type
alias, not the concrete subclass. Scoring a 2000-segment corpus:
21495µs → 2065µs, 10.4x.

Read this next to §10, where the same hypothesis about the same
mechanism was REFUTED — unboxing the lexer's chunks bought 8% where
23% was predicted. Both results are correct, and the difference is
the point: a scoring loop reads three components per iteration and
does nothing else, so per-element cost is the entire cost, while the
lexer does real work per character and boxing disappears into it.
The lesson is not "boxing is cheap" or "boxing is expensive" — it is
that neither generalizes, which is why both experiments exist.

That last per-query number was 49µs until this benchmark was written. The retriever
built a `Segment` — a substring of the source — for EVERY definition
matching the query, then took the top k; on a corpus where a common
name has hundreds of definitions, that was essentially all of its
cost. Replacing the collection pipeline with an `Iterator` so only the
k returned segments are ever cut made it **91x faster**. Nothing about
the design changed; the benchmark simply asked a question nobody had
asked, which is what benchmarks are for.

---

## 12. Consumption — where the boxing was, per 10k Longs

Every number here is 10 000 elements in chunks of 64, JMH, 3 forks
where a decision hung on it. The interesting part is that two
intuitions were wrong before the lanes were written, so the diagnostic
lanes matter as much as the result.

**A fold whose step is written at the call site.**

| lane | us/op |
|---|---|
| `Chunks.fold` + `Fold.sum[Long]`, before | 38.2 |
| `Chunks.foldLeft(p)(0L)(_ + _)` | **7.0** |
| the same step, via `Numeric.plus` | 8.2 |
| a hand loop over the same chunks | 2.6 |

`Numeric` survives inlining fine, so the typeclass was never the cost.
`Fold[A, S]` is `add(s: S, a: A): S`, generic in both, and a
megamorphic call site gives the JIT no way to remove the boxes.

**Which half of the boxing.** This is the lane that changed the design:

| lane | us/op |
|---|---|
| accumulator generic, element read directly | 29.4 |
| element boxed, accumulator a raw `long` | **2.8** |
| floor | 2.6 |

The accumulator is essentially the whole cost; boxing the element read
is nearly free. That is why only the accumulator is specialized, and
why the specialization is useful at all — `Chunks.fold` cannot know
the element type either way.

**A fold that arrives as data**, where nothing can inline — an
`Aggregator`'s, a java `Collector`'s:

| lane | us/op |
|---|---|
| a plain `Fold[Long, Long]` | 34.1 |
| `Fold.long(z)(f)`, constructor **not** inline | 27.5 |
| `Fold.long(z)(f)`, constructor inline | **7.8** |

The middle row is the trap: a plain constructor stores the step as a
`Function2`, whose `apply` erases generic, so the boxing the subtrait
just removed comes straight back in the field it closed over. `inline`
beta-reduces the lambda into `addLong` and there is no function object
left to call. The anonymous class is then duplicated per call site,
which is the mechanism rather than an accident.

**Aggregators**, after the specialization was carried up and the tuple
accumulators flattened:

| lane | before | after | floor |
|---|---|---|---|
| `count` | 37.8 | **19.5** | 8.6 |
| `sum` | 40.8 | **18.5** | 8.6 |
| `mean` | 87.0 | **37.3** | 18.6 |
| `variance` | 90.9 | **74.7** | 49.6 |

`Aggregator.fold` used to build a generic `Fold` unconditionally, so
none of the above reached Spark, Flink, the cluster or a java
`Collector` — the callers that can inline nothing and therefore need it
most. `count` was 5.5x slower than `Fold.count` for identical
arithmetic.

The tuple accumulators were the larger hole and were not obvious in
advance: `mean` carried a `(N, Long)` and `variance` a
`(Long, Double, Double)`, which is three and four allocations per
**element** — the tuple, plus a box per field, since a tuple's fields
are `Object`. Flat case classes with primitive fields cost one, and in
a local fold the JIT often drops even that. `variance` stays close to
its floor because Welford's per-element division dominates it, not the
accumulator.

## Why the good numbers, in one place

1. **No runtime where none is needed.** Pure binds are plain data
   (or, opted in, plain calls); fibers are virtual threads; blocking
   is parking. Every lane where competitors pay a scheduler/run-loop
   tax and okay doesn't traces to this.
2. **The rotation.** Left-nested binds rebalance tail-recursively in
   `fold` — the freer monad's classic quadratic trap (visible in the
   kyo Env/Emit lanes) never fires.
3. **Zero-allocation telling.** Writer's operation is an opaque
   identity signature: emitting a value allocates nothing.
4. **Relay for the one-shot majority.** Tail-resumptive handlers run
   as tail loops; the general (abortive/multi-shot) handler exists
   for the minority that needs it.
5. **Chunks amortize the tree.** One Free node per 64 elements, tight
   array passes between — program-as-value at near-array prices.
6. **Inline staging for known shapes.** When the pipeline is spelled
   where it runs, partial evaluation removes even the amortized cost
   — under the Iterator floor.
7. **The laziness contract is kept, not sold.** Construction does no
   work; the one encoding that trades it away (`Eager`) says so on
   the label. Several "slower" competitor numbers are actually THIS
   difference measured (see the kyo asterisks).
8. **The same incremental machine, at every layer.** The lexer's
   reconvergence and the parser's snapshot resume are one mechanism,
   and §10 and §11 are that one mechanism measured on JSON and on
   source code — 2.2x and 3.6x under a full re-run. Nothing in the
   retrieval layer had to invent incrementality; it inherited it.
9. **Cheap questions stay cheap.** Half a microsecond for an exact
   symbol lookup is what lets `Grounded.context` retrieve on EVERY
   turn instead of asking the model whether it should. Design
   decisions above depend on prices below being small.

## Where the numbers are honest about limits

- Microbenchmarks: naked plumbing, no real workloads; they price
  mechanisms, not applications.
- fs2/ZStream generator and merge lanes are their per-element worst
  cases (stated in place).
- JSON decode pays the totality/losslessness contract (stated
  above); CBOR and encode do not.
- The retrieval lane has no third-party comparison, deliberately: no
  Scala library ships this shape, and benchmarking a Python stack
  across a process boundary would measure the boundary. What it
  compares instead is our own two methods against each other, which
  is the choice a user of this library actually faces.
- Structural chunking is SLOWER than windowing (2.2x) and that is the
  intended trade; the quality percentage next to it is the other half
  of the number and should never be quoted apart from it.
- The host is a busy laptop; medians across forks and same-session
  grouping are the discipline, and history.tsv records the load.
