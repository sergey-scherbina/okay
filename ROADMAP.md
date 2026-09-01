# Roadmap

Every phase has a spec in specs/: modules-infra (P0),
cross-platform-async (the policy), aggregators (P1),
parallel-resilience (P2), interop (P3), external-systems (P4),
stage-pipeline + streaming-lex + streaming-parse + codecs (P5), llm,
staged-pipelines (P6), cluster (P7).

Decisions in force: the core module is plain `okay` (no suffix) — every
satellite carries one (`okay-cats`, `okay-kafka`, ...). Modules are
kept SMALL — the smaller the better, rare exceptions aside. groupId
`dev.okay` (build.sbt is the decision in force; it was
`io.sergiy-shcherbyna` until c2c6d87, and this line lagged behind). Scala: latest (3.7+). License: Apache-2.0. ScalaCheck allowed in
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

## P5 — The lex/parse/codec stack (three small modules + core support)
Specs: stage-pipeline.md, streaming-lex.md, streaming-parse.md.
- **Core**: `Stage[I, O, A]` (a transducer as a Take+Writer program)
  and `through` composition — demand-driven coroutine pipelines,
  chunked adapters. Tokenizers, parsers and dialects all share this
  shape.
- **okay-lex** — streaming tokenization: pure-state Scan step
  functions (state crosses chunk boundaries as a value), TOTAL (Error
  is a token channel, never a fault), exact spans, incremental
  relexing (snapshot + re-convergence) in the contract from day one.
  Also the interface BPE/SentencePiece implement in okay-llm.
- **okay-parse** — streaming error-tolerant parsing: TOTAL (any input,
  truncated included, yields a tree; errors are nodes with spans,
  diagnostics a data channel; Throws banned by design — which is what
  makes it the substrate for LLM streaming). TWO surfaces from the
  start, converging: the uniml VM (dialect = Stage of Open/Close/
  Emit/Reframe instructions, builder = total Fold to a lossless CST)
  and total parser combinators over Take — both compiling to the ONE
  instruction language. Incremental reparse (node-boundary snapshots,
  subtree reuse by reference, O(damage) work) in the contract.
- **okay-codec** — the dialects and semantic projections on top:
  JSON, XML, CBOR, Markdown, YAML; encoders as streams back;
  derivation via Scala 3 Mirrors (dependency-free). This is also what
  cross-platform client/server interop rides on.

## P6 — Staging, Catalyst-style
Reify Chunks pipelines as an operator tree (an initial encoding —
native ground for us), rewrite rules (map fusion, filter/take
pushdown), then whole-stage codegen: compile the whole pipeline into
one while-loop via inline/Expr (continues specs/staged-tagless.md).
Selective for static branching.

## P7 — Own distributed runtime
`okay-cluster`, its own spec: actors/nodes over Channel + Async +
codecs + transport; independent of the P4 bridges.

## P8 — Documentation (the user's standing ask)
Not one README but real documentation: a user guide, a tutorial, a
TYPEPEDIA (every type and typeclass of the library, its meaning, laws
and gotchas — the doc comments are already written in that spirit),
PER-MODULE documentation (every okay-* gets its own guide: what it
bridges, the idioms, the caveats), and the honest "why okay" comparison
(measured numbers, the laziness contract, the choice of encodings).
Likely a docs site generated from markdown + scaladoc; structure to
be specced when the phase starts.

## Ongoing — polish and optimization
API ergonomics (the inference gotchas, an import story), typeclass
law tests (ScalaCheck), scaladoc, releases. Optimization strictly by
the measured protocol; history and refuted experiments stay in
src/jmh/history.tsv.

## P9 — okay-agent: the agentic layer

Agents as programs (specs/llm-agentic.md): a tool call is an effect
operation, an agent is a Stage, the conversation is a FOLD
(`Aggregator[Turn, S, Seq[Turn]]` — incremental, mergeable,
backtrackable), search over completions is `Logic`, and every policy
question is a handler. v1 (effects, derived tool schemas, the
compacting context) and v2 (search strategies, state-threaded memory)
shipped 2026-08-30; the open items are lineage-backed tool results,
streaming validation that cuts generation, and the okay-langchain4j
interop that turns their providers into handlers for `Model`.

## P10 — okay-rag: retrieval from our own primitives

Retrieval designed against LangChain rather than after it
(specs/rag.md). Five things follow mechanically from what exists and
cannot be expressed in the frameworks we compared with: provenance by
construction (exact spans, lossless CST — citations that cannot
drift); incremental re-indexing at O(damage) over lex/parse
reconvergence; retrieval and chat memory sharing ONE budget and one
fold; passages kept as lineage (lossy in the view, lossless in the
source); and an index that is an Aggregator, so distribution and
incremental update are the same operation. Phases, all shipped 2026-08-31: documents and splitting (a),
embeddings and the store INTERFACE (b), retrieval pipelines with fair
interleaving and fusion (c), resilient ingestion and
damage-proportional re-indexing (d), keyword/hybrid (e), and code as
the proving corpus (f) — definitions with their doc comments, a
symbol index that is a Monoid, and retrieval-augmented `recall` in
okay-agent, where conversation and code share ONE budget and the
common case costs no tool call.

Note what is deliberately absent: no Runnable/LCEL layer — flatMap,
Stage/through and Chunks already are invoke/stream/batch, typed.

## P11 — okay-mcp: the Model Context Protocol

Both ends (specs/mcp.md), shipped 2026-09-01. The design was already
decided in specs/llm-agentic.md — "an MCP server is another `Tool`
handler, and its JSON-RPC framing is our total parser plus `Schema`" —
and the module is small because that sentence is true: an agent
program is unchanged when its tools come from a server, and serving
our own tools is a `Stage[Rpc, Rpc, Unit]` with the transport outside
it. v1 was tools end to end over stdio; v2 added resources and prompts the
same day, and each lands on a type that already existed — a resource
is `okay.rag.Source` (so a server's documents go through the
retriever), a prompt is `Seq[Turn]` (so a server's prompt is an agent
opening). v3 made the session duplex — the server talking first: subscriptions,
roots, and sampling, where `sampling/createMessage` is the `Model`
effect, so an MCP server borrows the client's own model handler. What
remains is elicitation (which needs a UI contract) and the
streamable-HTTP transport.
