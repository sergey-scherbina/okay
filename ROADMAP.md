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
`okay-scala2`, the language itself as interop (specs/scala2-facade.md,
stages 1–15 landed 2026-09-23): a Scala 2.13 build gets effects, open
rows, its own effects, `Cont`, streams, fibers and channels, and in
sixteen more `okay-scala2-*` modules (under `scala2/`) the rest of the
library, from codecs and HTTP to durable workflows, optics and the
service libraries, through types scalac 2.13 reads with
`-Ytasty-reader`. It is a facade, not a cross-build: the row is a union
and the combinators are `inline`. Not wrapped, by necessity: the interop
modules (their libraries' `_3` artifacts conflict with a 2.13 build) and
Scala 3 metaprogramming (direct style, staging).

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

## Continuations — the four roads (specs/continuations-roadmap.md)
With `Cont` a facade over `Free` (theory ch. 11), the plan for the
continuation machinery is one spec with four ranked roads, each owing
a number: a fused walk over a whole row (`handlers-fused-walk`, capped
by handler-fusion's measured 1.1–1.3x), staged `direct` blocks with no
tree when the handlers are static (`direct-staged`, ceiling 1.9x
measured for the hand-written shape), typestate on a facade
(`freer-base-stage2`, a compile error as the deliverable), and
continuations as data as a spike before anything is planned. The
bounding fact is written there too: a freer tree is a coroutine, not a
syntax tree, so compilation lives at construction, in the interpreter,
and in the first-order DSLs — never on a built tree.

## P9 — okay-agent: the agentic layer

Agents as programs (specs/llm-agentic.md): a tool call is an effect
operation, an agent is a Stage, the conversation is a FOLD
(`Aggregator[Turn, S, Seq[Turn]]` — incremental, mergeable,
backtrackable), search over completions is `Logic`, and every policy
question is a handler. v1 (effects, derived tool schemas, the
compacting context) and v2 (search strategies, state-threaded memory)
shipped 2026-08-30. All three opens closed 2026-09-01:
streaming validation that cuts generation (okay-llm `Cut`, Delim as
the mechanism — and the repair door `screened` joined it via
conditions), the okay-langchain4j interop (their ChatModel as a
`Handler[Model]`), and lineage-backed tool results
(`Large.projecting` wraps any tool handler: an oversized result is
stored whole, the context gets head+handle+size, the `expand` tool
reads windows on demand — llm-agentic.md's box checked, TestLarge).

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
effect, so an MCP server borrows the client's own model handler. Both remaining
opens closed since: elicitation end to end via okay-ui's contract
(P12 v1), and the streamable-HTTP transport as `McpHttp` in
okay-http (route + link, session ids, the 404 reinitialize signal —
beside the WebSocket transport), spec'd in specs/mcp.md.

## P12 — okay-ui: the toolkit that is not a toolkit

The view as a value, the loop as `transduce` over merged sources, the
renderer as a seam (specs/ui.md). v1 shipped 2026-09-01: terminal +
React-shaped hosts, the diff, forms from Schema, and MCP elicitation
closed end to end. Above v1, designed and backlogged: scenarios as
programs (Dialog), screens as a stack, server-driven UI over the wire
("the tree is the capability list"), event-sourced sessions.

## P13 — where the effort goes next (the operator's order, 2026-09-18)

Asked directly: what else do the applicative-shaped classes buy, can
we compete with Spark and Flink, and what is missing where. The answer
below is argued from measurements this repository keeps, and the ORDER
is the operator's.

**1. `Validated` — every error, not the first (specs/validated.md).**
The classic applicative payoff Okay does not have. `Throws` is
monadic, so it stops at the first error; an applicative cannot bind
one leaf's answer into another's body, so it has no way to stop and
therefore collects. Three consumers are worse today for the lack:
`okay-conf` reports one missing key per run, schema validation has
`form-errors-on-validate` waiting in BACKLOG for exactly this type,
and `okay-openapi` rejects a body at its first bad field. Small, and
useful the day it lands.

**2. Durable workflows as the flagship, ahead of dataflow.** The
market is younger, the moat is smaller, and the differentiation is
sharper: Temporal and its kin need determinism BY CONVENTION, while a
captured continuation makes replay typed. `Wf`, `Replayable`, the
dialogue journal, the worker and coordinator recovery are already
built (specs/durable-workflow.md), and "a continuation journal is
event sourcing" is the idea the rest of the field does not have. This
is the bet with the best ratio of what is built to what is left.

**3. Dataflow: the EMBEDDED tier with a published ceiling, not a race
for nodes.** Measured on one machine, the Wrocław job, 2.4 million
events (docs/benchmarks.md §20): okay 22 561 859 ev/s at 8 cores,
Flink 1.20 at 1 679 971, Spark 4.0 batch RDD at 338 918 — with three
of the five stages being literally the same code in both lanes, so
what differs is the engine and not the arithmetic.

That is not a licence to claim a win. Flink pays for its own
scheduling, checkpointing and serialization, and stage 12 of
specs/dataflow.md is BLOCKED for want of machines that are not this
one, so "distributed" is a claim and not yet a result. And the moat is
not the engine at all: it is a hundred connectors, SQL, a catalog and
the operational tooling. Chasing that head-on is years for no
advantage.

What the numbers DO say: 107 ms for 2.4 million events on one machine
means the job people run on a twenty-node cluster fits in one server.
Most streaming work sits far below the size that justifies a cluster
and pays for one anyway. So the position is **Flink's semantics as an
embeddable library with no cluster to run** — event time, watermarks,
keyed state, exactly-once, and no daemon. It is what DuckDB did to
Spark for analytics, and `okay-delta` already uses DuckDB as its read
road. Three things follow, and they are backlogged rather than
promised: publish the honest CEILING (the throughput and state size
past which a cluster is the right answer — Flink never publishes one
and we can measure ours); make the migration seam a documented path,
since `okay-flink` already proved `Aggregator` is an
`AggregateFunction` field for field and that it survives
serialization into a job graph; and finish stage 12 at the scale of a
few machines rather than a thousand.

**4. Capability lists from `Static`.** `Static.leaves`
(specs/applicative-static.md) answers what a program MAY perform
before it runs, which is what `okay-di` currently asks authors to
declare by hand. Deriving needs from the program is the honest
version of "needs declared where the thing opens".
DONE 2026-09-18 (di-needs-from-static): `Provision` is the
deployment's vocabulary as an effect signature, `Needs.provisioned`
takes a `Static` spine over it and reads the needs off its leaves,
and the thing opens with the place's answer — the demo's store is
written this way and declares nothing by hand.
