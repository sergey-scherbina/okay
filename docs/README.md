# Okay — documentation

Okay! Extensible effects for Scala 3, founded on the parameterised
continuation monad. Zero dependencies in the core; one source for
JVM (JDK 21+, Loom), Scala.js and Scala Native.

## Start here

- **[User guide](guide.md)** — the concepts, layer by layer: control
  → effects → streams → chunks → coroutines → concurrency → the text
  stack → the laziness contract that holds it together.
- **[Tutorial](tutorial.md)** — the same layers by use, twenty-two
  worked chapters from a pure program to an agent with remote tools;
  every snippet's shape runs in the repo's tests.
- **[Building a chat application](building-a-chat-app.md)** — from an
  EMPTY DIRECTORY to a running streaming chat, outside this
  repository: how to depend on a library that is not published yet
  (the awkward step, three roads, one recommended), the backend route,
  the Scala.js frontend whose logic is tested on the JVM, the tests
  over a real socket, and running it. Every command executed before it
  was written down.
- **[The theory of Okay](theory/index.md)** — the textbook: which
  theories the library stands on, who established them (Moggi, Wadler,
  Felleisen, Danvy & Filinski, Atkey, Swierstra, Kiselyov, Plotkin &
  Power/Pretnar, Carette–Kiselyov–Shan, Taha & Sheard), and why each
  design decision — argued from the papers and the repository's own
  measurements. Nine chapters, Okay as the running example.
- **[Direct style](direct-style.md)** — monads as plain code:
  the reflection foundation (two one-liners, no macros), the
  `direct` block (one mark, `.?`), auto-coloring behind two explicit
  gates, do-notation statements — with the rationale for every
  boundary and the graveyard of refuted alternatives. The theory
  chapter with the literature is [theory/08](theory/08-direct-style.md).
- **[Channels and queues](queues.md)** — the two decisions a channel
  is (contract, then mechanism), every implementation and what it
  trades, the builder, recipes, the measured table, and the
  literature: Vyukov, Michael & Scott, Okasaki, Koch–Sanders–Williams
  on relaxed FIFO, Herlihy & Shavit.
- **[Typepedia](typepedia.md)** — every core type and typeclass with
  its meaning and the recurring gotchas; the reference you grep.
- **[Capabilities](capabilities.md)** — context functions as the
  wiring: doors, `provide`/`providing`, `wire`, the zero-framework
  dependency-injection story, the theory the compiler runs (the
  Reader monad, with the elaborator as its interpreter), and the
  exact boundaries — every claim traced to a compiled experiment.
- **[The cast that could not go, and how it went](existentials.md)** —
  six encodings tried against the assertion behind `Writer.tell`, what
  the compiler said to each, and the bytecode showing why five of them
  fail for one mechanical reason. The sixth worked; the failures are
  the useful part.
- **[Benchmarks](benchmarks.md)** — every measured case with its
  table, WHY the Okay number is what it is, why the competitors'
  numbers differ, and where the honest limits are. Raw history with
  protocols and refuted experiments: [history.tsv](../src/jmh/history.tsv).

## The modules

Each page is the module's full documentation: guide, tutorial,
API reference, gotchas.

| module | what it is |
|---|---|
| `okay` (core) | effects, streams, chunks, the algebra — covered by the guide/tutorial/typepedia above |
| [`okay-cats`](modules/okay-cats.md) | cats instances (law-tested), IO and free-monad bridges, their runtime as our Scheduler |
| [`okay-zio`](modules/okay-zio.md) | ZIO and ZStream bridges, the ZIO scheduler |
| [`okay-kyo`](modules/okay-kyo.md) | kyo bridges and the structural effect-row mapping |
| [`okay-fs2`](modules/okay-fs2.md) | fs2 streams, chunk for chunk, native backpressure both sides |
| [`okay-kafka`](modules/okay-kafka.md) | Kafka: one poll, one chunk; offsets = the replayable capability |
| [`okay-spark`](modules/okay-spark.md) | Spark via the Aggregator triple — one value, local or distributed |
| [`okay-flink`](modules/okay-flink.md) | Flink via the same triple |
| [`okay-jdbc`](modules/okay-jdbc.md) | JDBC as chunked streams under the Resource region |
| [`okay-r2dbc`](modules/okay-r2dbc.md) | the R2DBC hatch behind the same Sql seam: driver availability on the JVM, honestly framed as not a speed unlock |
| [`okay-delta`](modules/okay-delta.md) | Delta Lake without Spark: Delta Kernel create/append/snapshot/rows over SqlValue rows; DuckDB reads the same table through the JDBC seam |
| [`okay-lex`](modules/okay-lex.md) | total streaming tokenization: chunked, snapshottable, incremental |
| [`okay-parse`](modules/okay-parse.md) | total lossless parsing; incremental reparse with reference reuse |
| [`okay-codec`](modules/okay-codec.md) | the Schema algebra; JSON, CBOR and Markdown dialects |
| [`okay-llm`](modules/okay-llm.md) | language models as streams; two protocols over one seam; structured output that cuts generation |
| [`okay-agent`](modules/okay-agent.md) | agents as programs: tools as operations, context as a fold, search as Logic |
| [`okay-frame`](modules/okay-frame.md) | what a form is: named slots, typed answers, the language of the exchange, and what is still missing |
| [`okay-intent`](modules/okay-intent.md) | a message to a class and a filled frame, with a model or without one: one Schema is the taxonomy, the frame and the parser |
| [`okay-rag`](modules/okay-rag.md) | retrieval: split the tree not the string, code in eight languages indexed by parsing it, symbols without embeddings |
| [`okay-cluster`](modules/okay-cluster.md) | the remote channel, distributed chunk work, the JS↔JVM acceptance |
| [`okay-http`](modules/okay-http.md) | REST and WebSocket as programs: a body is a `Source`, a socket session is a `Stage[Frame, Frame, A]`, and a socket IS an MCP `Link` |
| [`okay-jetty`](modules/okay-jetty.md) | Jetty behind the same two seams — and the WebSocket SERVER okay-http could not serve |
| [`okay-netty`](modules/okay-netty.md) | Netty behind the same two seams, plus the cross-backend matrix that proves the seam |
| [`okay-security`](modules/okay-security.md) | authorization once: claims as values, JWT/JWKS over a crypto seam, PBKDF2, policies as an algebra, routes wrapped so a principal must exist, OAuth2 client flows |
| [`okay-crypto`](modules/okay-crypto.md) | the primitive crypto seam — MAC, hash, KDF, randomness on the platform's own crypto, so okay-pg's SCRAM need not cycle through the security stack |
| [`okay-ui`](modules/okay-ui.md) | the toolkit that is not a toolkit: the view is a value, the renderer is a seam — terminal, React, test host, one application; forms derived from Schema |
| [`okay-mcp`](modules/okay-mcp.md) | the Model Context Protocol, both ends: a server is a `Handler[Tool]`, our tools are a server, and the protocol is a pure Stage |
| [`okay-persist`](modules/okay-persist.md) | the durable log: one primitive, staged — segments and recovery, offsets, compaction, replication's core, Sql/Kafka store engines, the Doctor |
| [`okay-ops`](modules/okay-ops.md) | health, stats and Prometheus over the persist log's own values: /healthz, /readyz, /stats, /metrics — no SDK, the manifest is the Kubernetes integration |
| [`okay-deploy`](modules/okay-deploy.md) | a deployment as a value: pure renderers to a Dockerfile, a Helm chart and a compose file; an app declares its Deploy and owns the rendered files |
| [`okay-cache`](modules/okay-cache.md) | how a cache is ALLOWED to be wrong, named: budgets, invalidation, the log-fed view; memory and Redis engines; the cross-node invalidation topic |
| [`okay-sql`](modules/okay-sql.md) | the relational seam: SqlValue/Col and the typed layer once, drivers underneath (JDBC, the pg wire, sqlite) |
| [`okay-pg`](modules/okay-pg.md) | the Postgres v3 protocol spoken natively: SCRAM (phase objects), the extended protocol, no JVM driver in between |
| [`okay-docs`](modules/okay-docs.md) | the document seam: get/put with CAS, declared consistency; TopicDocs own engine |
| [`okay-docs-mongo`](modules/okay-docs-mongo.md) | the foreign adapter that proves the Docs seam, on Mongo's native CAS |
| [`okay-conf`](modules/okay-conf.md) | configuration as data, secrets as REFERENCES — a config cannot leak what it does not contain |
| [`okay-tls`](modules/okay-tls.md) | one TLS seam at the transport; the sslmode ladder, verify-full the only default; keys as Secret refs |
| [`okay-mail`](modules/okay-mail.md) | sending mail: SMTP over a socket with STARTTLS, send only, and failure as data |
| [`okay-blob`](modules/okay-blob.md) | the object-store seam: fs and S3 engines, OWN SigV4 pinned by the AWS vectors, persist backups |
| [`okay-obs`](modules/okay-obs.md) | tracing without a framework: spans as values on a topic, W3C traceparent, capability routes, OTLP export as a consumer |
| [`okay-py`](modules/okay-py.md) | Python as a handler: operations not eval, a clean-env shim with a version handshake, N workers past the GIL |
| [`okay-script`](modules/okay-script.md) | markdown files as Scala source: fenced blocks through the real Scala 3 compiler in-process, errors pointing at the original `.md` line |
| [`okay-langchain4j`](modules/okay-langchain4j.md) | their ChatModel as a `Handler[Model]` — their provider breadth behind our effect |
| [`okay-langchain4j-embed`](modules/okay-langchain4j-embed.md) | their EmbeddingModel as `String => Embedding` and as okay-rag's `Handler[Embed]` |
| [`okay-demo-e2e-browser`](modules/okay-demo-e2e-browser.md) | one chat round through a real headless browser — the fetch/ReadableStream glue a JVM test cannot reach |
| [`okay-security-argon2`](modules/okay-security-argon2.md) | the one satellite that buys a dependency: Argon2id in the PHC form, RFC-vector-pinned |
| [`okay-java`](modules/okay-java.md) | the JDK itself as interop: an Aggregator IS a Collector |
| [`okay-chat`](modules/okay-chat.md) | a streaming LLM chat component: the model seam, Cut-guarded SSE framing, the /chat route — extracted from the demo |
| [`okay-admin`](modules/okay-admin.md) | protected admin routes over the same bearer-token 401/403 ladder as every other protected route |
| [`okay-subscription`](modules/okay-subscription.md) | gate a resource behind a paid period: free for the join month, then paid-this-period or gated, never deleted |
| [`okay-live`](modules/okay-live.md) | broadcast (Hub) and per-key channels (Registry) over the core's own Channel |
| [`okay-demo`](modules/okay-demo.md) | not a library: a coding agent over THIS repository, built from the public surface as a user would (`sbt 'okayDemo/runMain okay.demo.RepoAgent <question>'`); `RepoMcp`, the same repository served as an MCP server (tools, every file as a resource, an `explain` prompt); `ChatDemo`, the chat that runs the stack (streamed replies cut by a guard, a durable task board the model drives through tools, assignments ringing live pages — works with no model); and the worked examples — `Combine` joins two live telemetry streams twice, `Stage.transduce` against fs2's `mapAccumulate` shape, with tests pricing the difference |

## How the claims are checked

Three kinds of test, deliberately different consumers of the same
code. EXAMPLES (the bulk) state what a piece should do. PROPERTIES
(`TestLaws` in five modules) hand it generated input, which agrees
with nothing by construction — they have found lost and duplicated
tokens in lex reconvergence, a widened passage that no longer
contained its passage, and a documented claim that was simply false.
And an APPLICATION (`okay-demo`) assembles the public surface the way
a user would; its first run found a loader that indexed nothing and
an API with no way to see what it sent.

A fourth kind runs only where the world cooperates: ACCEPTANCE
against things nobody here wrote — a real model behind an
OpenAI-compatible endpoint (okay-agent's `TestLive`), the MCP
reference server spawned by npx (okay-mcp's `TestLive`), a Node
client against a JVM server (okay-cluster). Skipped, not failed,
where the endpoint or runtime is absent; what they test is OUR
assumptions, not the other side's behaviour.

## Design documents

[`specs/`](../specs) holds one spec per feature with behavior
checkboxes (all closed), the decisions, and the experiments that
were tried, measured and REJECTED — so nobody re-runs them blind.
Start with [the roadmap](../ROADMAP.md) for the shape of the whole.

## The papers underneath

- Oleg Kiselyov, Hiromi Ishii —
  [Freer Monads, More Extensible Effects](https://okmij.org/ftp/Haskell/extensible/more.pdf).
  The freer monad and extensible-effects design the effect layer
  reenacts (with `Free` and `Eff` literally replaying the 2015-tree
  vs 2013-continuation history).
- Robert Atkey —
  [Parameterised notions of computation](https://bentnib.org/paramnotions-jfp.html).
  The parameterised (answer-type-changing) monad `Cont[A, S, R]` is
  founded on.
- Rúnar Óli Bjarnason —
  [Stackless Scala With Free Monads](https://blog.higher-order.com/assets/trampolines.pdf).
  Why stack safety on the JVM means trampolining through data — the
  reason `Cont` and `Free` are defunctionalized enums with
  tail-recursive runners rather than raw closures.
- Oleg Kiselyov et al. — the delimited-control lineage (`shift`/
  `reset`) that makes handlers literally continuations (`F !> S`).

## Orientation, briefly

Programs are VALUES (`A ! F`): construction does no work, running is
interpreting. Handlers are continuations. Streams are codata (one
observation: `uncons`). Chunks amortize the tree. The laziness
contract is load-bearing — it is what makes handlers stream
transformers, chunk retry a lineage recompute, and the whole
lex/parse stack incremental. When a shape is known at the call site,
inline staging removes even the amortized cost. Everything else is a
consequence.
