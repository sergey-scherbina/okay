# Roadmap

Decisions in force: the core module is plain `okay` (no suffix) — every
satellite carries one (`okay-cats`, `okay-kafka`, ...). groupId
`io.sergiy-shcherbyna` (domain verification to be settled by publication
time). Scala: latest (3.7+). License: Apache-2.0. ScalaCheck allowed in
test scope only; the core stays dependency-free.

## The cross-platform policy

One and the same source runs on JVM, JS and Native, using each
platform's abilities transparently and fully — and programs on
different platforms interoperate (a client on one, a server on
another). Designed up front so nothing has to break later:

- Programs stay in the effect world (`A ! Async` composes by flatMap —
  non-blocking by construction); blocking appears only at the run
  boundary, and only where the platform has it.
- `Async` grows a callback operation (`Await(register)`) beside the
  thunk one: awaiting is universal; a blocking thunk is a JVM/Native
  capability. On JVM the runner parks a virtual thread (Loom), on
  Native a thread, on JS the runner drives the tree through the event
  loop (`runAsync: Future[A]` — a different terminal, the same
  programs).
- Blocking `Fiber.join` is a JVM/Native capability (evidence-gated);
  the cross-platform fiber surface is completion/cancellation.
- Cross-platform interop between running programs = codecs (below) +
  a transport module.

## P0 — Module infrastructure
sbt restructure: `okay` (the core, crossProject JVM/JS/Native; Async/
Channel/Fiber blocking parts in jvm+native source sets), satellite
stubs, `compare` stays an internal benchmark module. CI (test +
Jmh/compile), publish settings, LICENSE.

## P1 — The data-analysis algebra
`Group` (Monoid with inverse); `Aggregator[-In, Acc, +Out]` = `Fold` +
present, with `zip` (two statistics, one pass), `map`, `contramap`;
the standard library: mean, variance/stddev (Chan/Golub/LeVeque),
min/max/first/last/topK, groupBy (Map[K, Acc] as a Monoid); sliding
windows on Group over our streams (subtract what aged out — Chunks +
Async ticks). P1b: sketches written fresh and idiomatically —
HyperLogLog, Count-Min, T-Digest as approximate monoids with stated
error. Terminals: `Chunks.fold(agg)`, `Stream.fold(agg)`.
(The design source is scalascript's aggregation-algebra spec; the
implementation is ours.)

## P2 — Parallelism and resilience
`parMap`/`parTraverse` over Chunks (a chunk per fiber — the natural
grain); pipeline parallelism (a stage per fiber, Channels between);
backpressure audit (park-based bounded channels, chunk-aware
capacities). Fault tolerance PER CHUNK, Spark-style: our streams are
pure programs and re-observation recomputes — that IS lineage, so a
chunk is the unit of failure and recompute; retry policies as streams
of delays; checkpointing = memoized prefixes; fiber supervision.

## P3 — Interop modules
`okay-cats` (Monad/MonadError instances for `A ! F`, Async ⇄ IO),
`okay-zio` (Async ⇄ ZIO, ZStream ⇄ Chunks), `okay-kyo`, `okay-fs2`
(Stream ⇄ Chunks via uncons, both directions).

## P4 — External systems
`okay-kafka` first (consumer polling is chunked by nature; source/sink
as `Chunks[A] ! Async` + Resource); `okay-spark`/`okay-flink` via the
Aggregator → (zero, seqOp, combOp) bridge; JDBC; `okay-llm` — a thin
streaming client (tokens as `Chunks[Token] ! Async`, retries from P2)
designed to grow the agentic layer later (see ../rozum for the larger
shape; not urgent).

## P5 — Codecs: uniml, redesigned
`okay-codec`: the uniml idea (one token-to-tree model shared by JSON,
XML, CBOR, Markdown, YAML dialects; lossless CST; semantic
projections) rebuilt idiomatically and immutably on our own machinery:
the token stream is `Chunks[Token]`, the tree builder is a
Fold/handler, dialects are stream transformers, encoders are streams
back. Derivation via Scala 3 Mirrors (dependency-free). This is also
what cross-platform client/server interop rides on.

## P6 — Staging, Catalyst-style
Reify Chunks pipelines as an operator tree (an initial encoding —
native ground for us), rewrite rules (map fusion, filter/take
pushdown), then whole-stage codegen: compile the whole pipeline into
one while-loop via inline/Expr (continues specs/staged-tagless.md).
Selective for static branching.

## P7 — Own distributed runtime
`okay-cluster`, its own spec: actors/nodes over Channel + Async +
codecs + transport; independent of the P4 bridges.

## Ongoing — polish and optimization
API ergonomics (the inference gotchas, an import story), typeclass
law tests (ScalaCheck), scaladoc, releases. Optimization strictly by
the measured protocol; history and refuted experiments stay in
src/jmh/history.tsv.
