# Okay — documentation

Okay! Extensible effects for Scala 3, founded on the parameterised
continuation monad. Zero dependencies in the core; one source for
JVM (JDK 21+, Loom), Scala.js and Scala Native.

## Start here

- **[User guide](guide.md)** — the concepts, layer by layer: control
  → effects → streams → chunks → coroutines → concurrency → the text
  stack → the laziness contract that holds it together.
- **[Tutorial](tutorial.md)** — the same layers by use, twenty-five
  worked chapters from a pure program to an agent with remote tools;
  every snippet's shape runs in the repo's tests.
- **[Your own effect](your-own-effect.md)** — one worked effect from
  the enum to four interpretations of the same program: `derives
  Effect` and what it writes, constructors or `perform`, rows
  (`plus`/`at`), a step the program may DECLINE (`Abort`, a refutable
  pattern, `ensure`, `recover`), handlers against a real SQLite file
  and against a store type class, recording as a decorator,
  interpreting one effect INTO others, several instances of one
  signature (`Tag`, `Refs`) — and the four things that bite, each with
  the measurement behind it.
- **[Building a chat application](building-a-chat-app.md)** — from an
  EMPTY DIRECTORY to a running streaming chat, outside this
  repository: how to depend on a library that is not published yet
  (the awkward step, three roads, one recommended), the backend route,
  the Scala.js frontend whose logic is tested on the JVM, the tests
  over a real socket, and running it. Every command executed before it
  was written down.
- **[Declaring an API](declaring-an-api.md)** — a path, a query, a
  body and a tool, each written once and then asked several different
  questions: does this url match, what is the url for these
  parameters, and what does it look like with no request in hand. The
  law that makes it worth reifying, the three spellings the language
  refused, and the defects the conversions turned up.
- **[Cursors and declarations](cursors-and-declarations.md)** — the
  map of one arc: a consumer that is a program (`Pull`, `Take.each`),
  a cursor over a tree (`Zipper`, `JsonEditor`), a cursor whose
  position is a type (`TypedZipper`, its path as a form's key), where
  a cursor meets a form (`Form.drill`), and three declarations two
  interpreters read with a law between them (`Policy`, `Query`,
  `Toolbox.In`) — with the table of when to reach for which.
- **[Frontends: one application, any client](frontend-guide.md)** —
  the view as a value with two vocabulary levels, the hosts (terminal,
  React, DOM, Swing, GTK), the server-driven wire and its rendered
  contract, the hybrid rule (no round trip per keystroke), Live pages,
  the mobile web (installable, offline) and the native thin clients
  (Compose and SwiftUI). A browser draws a real `<table>`; a text says
  what it IS (an identifier, a number) and a host sets it accordingly;
  the token stylesheet ships with the tree.
- **[A site out of markdown](okay-script-guide.md)** — from an empty
  directory to a running shop whose pages ARE markdown files compiled
  at runtime: routing by directory, sessions and typed forms, a live
  page, two languages, a login, and the container. Every command in
  it was executed before it was written down.
- **[okay2: the core in Scala 2.13](okay2.md)** — not the facade
  but the core itself written a second time: the freer tree, rows as
  kind-`*` types, handlers in any order by a type-level witness,
  `Cont`, State/Writer/Throws/Reader, with nothing of Scala 3 on the
  classpath; interop with cats, fs2 and zio by interpreting the tree
  in the target monad; what is different from Scala 3 and why, each
  measured.
- **[okay from Scala 2.13](scala2.md)** — for a codebase still on
  Scala 2: the build (two standard libraries, and in which order),
  `Prog` and `Eff` (several effects in one program, the row as an
  intersection of capabilities), your own effect as a sealed trait and
  an object, continuations, streams, fibers and channels, and the rest
  of the library in seventeen modules (codecs, HTTP, WebSockets, SQL,
  agents, UI, resilience, the durable log, STM, stores, models,
  retrieval, MCP, optics, durable workflows, actors and the service
  libraries), a Scala 3 / Scala 2 phrasebook, and every compiler error the setup can produce
  with its cause. Every snippet compiles under scalac 2.13 with
  `-Werror` in the gate.
- **[okay with TypeScript](typescript.md)** — Scala and TypeScript in
  the three places they meet: a Scala backend with a TypeScript frontend
  (a typed `client.ts`, live documents typed by path), both in the
  browser (Scala.js exports with their `.d.ts`, the `@okay/ts` npm
  package, durable flows in IndexedDB, `Direct.ts { }`), and both on the
  backend (a Node worker, a generated Scala facade). Types are written
  once, in either language, and every generated file is checked by
  `tsc --strict`.
- **[okay with Go](go.md)** — Go programs as okay programs on a worker
  process: programs as data, multi-shot continuations, typed operations
  generated from the Scala callbacks, panics as conditions.
- **[okay with Rust](rust.md)** — Rust kernels as okay effects: a
  crate over the C ABI, bound through FFM, answering the same bytes as
  the JVM implementation (Argon2id); and why Go is a subprocess or Wasm,
  not in-process.
- **[okay with Python and R](python-and-r.md)** — Python and R
  functions as typed Scala functions, and their code calling back into
  okay's effects by name, with diagrams; held models, inline modules,
  streams, declared environments, a journal.
- **[okay with other JVM languages](jvm-languages.md)** — Java streams,
  Clojure and Frege together with okay: a stage IS a JDK gatherer and a
  Clojure transducer, okay's effects enter Clojure and Frege as a small
  program-as-data library in THAT language (never as lazy IO), lazy data
  crosses both ways, and the rules, costs and literature behind it.
- **[The theory of Okay](theory/index.md)** — the textbook: which
  theories the library stands on, who established them (Moggi, Wadler,
  Felleisen, Danvy & Filinski, Atkey, Swierstra, Kiselyov, Plotkin &
  Power/Pretnar, Carette–Kiselyov–Shan, Taha & Sheard, McBride &
  Paterson, Mokhov et al., Turner), and why each
  design decision — argued from the papers and the repository's own
  measurements. Thirteen chapters, Okay as the running example.
- **[Direct style](direct-style.md)** — monads as plain code:
  the reflection foundation (two one-liners, no macros), the
  `direct` block (one mark, `.?`), auto-coloring behind two explicit
  gates, do-notation statements — with the rationale for every
  boundary and the graveyard of refuted alternatives. The theory
  chapter with the literature is [theory/08](theory/08-direct-style.md).
- **[Continuations: a working book](continuations/index.md)** — the
  long form, written to be read straight through: why a team should
  care (answerable to a manager), the four shapes as recipes, how the
  machine works, how to build new effects on it, what it costs with
  numbers, what it must not be asked to do, and the production systems
  in this repository that are built from nothing else. Self-contained;
  it repeats what it needs.
- **[Continuations in practice](continuations-in-practice.md)** — the
  four shapes that earn a capture in ordinary code (`exit`, `collect`
  / `emit`, `resumable` / `pause`, `onReturn`), each beside the way it
  is usually written, the rule for when to reach for an effect
  instead, and the cases where a capture makes code worse. The theory
  is [theory/2](theory/02-continuations.md).
- **[Reading a blockchain](cardano.md)** — Cardano from a relay to
  typed tables with no node, no API key and no Spark: follow the chain
  to confirmed blocks (rollbacks said, never papered over), explode each
  block into transactions, inputs, outputs, assets, mints,
  certificates, withdrawals and redeemers, and read sum types and
  recursive datums as columns any engine can take — Spark when you
  want it. Its code is run by `TestCardanoGuide` on a recorded preprod
  session checked against Koios.
- **[Durable workflows](durable-workflows.md)** — a program that WAITS
  (for a person, a service, a date), written as straight-line code and
  able to outlive the process running it: the journal that is the only
  state, the four rules, and the engine over it —
  `sleep`/`awaitSignal`/`patch`, cancellation you can replay, bounded
  history (`continueAs`), child runs, retirement (which code is still
  reachable from a live journal), an advisory lease, and a resume cache
  that replays a dialogue once instead of once per call. It ends with
  an honest list of what a workflow ENGINE has that this does not. Its
  code is compiled by `TestWorkflowGuide`, so the page cannot drift
  from the library.
- **[Static workflows](static-workflows.md)** — the same workflow as a
  TERM rather than a monadic program: `Proc`, the free arrow over the
  questions the engine already journals, written in the same
  straight-line block (`Proc.direct`). What the shape buys is what a
  closure cannot give — `leaves` before the run, a deploy check that
  asks live journals whether they still fit the new code, a position
  that is a path and can be drawn, and an `Iter` node so a loop's trip
  count may still be an answer. One journal format underneath: a run
  started monadically is carried on by a term.
- **[okay-actor](modules/okay-actor.md)** — actors as composition: the
  mailbox is a `Channel`, one-at-a-time is one consumer, and the only
  new thing is supervision. With the four decisions it makes, and the
  alternatives each rejects.
- **[okay-reactive](modules/okay-reactive.md)** — Reactive Streams
  interop over the JDK's own `Flow`, with the TCK passing: what the
  specification demands, and the three things it caught that reading
  the prose did not.
- **[Channels and queues](queues.md)** — the two decisions a channel
  is (contract, then mechanism), every implementation and what it
  trades, the builder, recipes, the measured table, and the
  literature: Vyukov, Michael & Scott, Okasaki, Koch–Sanders–Williams
  on relaxed FIFO, Herlihy & Shavit.
- **[Schedulers](schedulers.md)** — the family (Loom, owned workers,
  a given pool, platform threads), the builder that chooses and tunes
  one, the single decision the owned-worker scheduler makes for itself
  by measuring its own last sixteen tasks, what blocking inside a
  worker costs and what `adaptive` buys back, the nine laws every
  member owes, and the numbers against kyo and the JDK pool.
- **[Several instances of one effect](many-instances.md)** — two
  counters in one row: why a bare row holds one of each signature, and
  the three routes that lift it (a key with `Tag`, a cell with `Refs`,
  a fresh `Delim` prompt), with what each costs.
- **[Arrows](arrows.md)** — a computation you can see before you run
  it: the glyphs (`>>>`, `&&&`, `+++`, and why Kleisli's is `>=>`),
  two scanners over one input in a single pass, where optics and
  arrows meet, and where optics and streams deliberately do not.
- **[Optics](optics.md)** — naming a path once: the nested `copy`, the
  `Option.map` chain, the `case s => s` in a rewrite, each beside the
  optic that replaces it; what each costs, measured; and the one pair
  where the `copy` is still the right code. Every block on the page is
  run by a test.
- **[Typepedia](typepedia.md)** — every core type and typeclass with
  its meaning and the recurring gotchas; the reference you grep.
- **[Dependency injection](di.md)** — the module vocabulary: a
  capability that must be opened and closed, a graph that IS the
  composition, qualifiers as types, reading a module before it is
  built (`plan`, `exports`, declared needs), an application wired end
  to end, the same modules inside Spring, Guice, CDI and ZIO, and
  what a deployment reads from all of it.
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
- **[The Wrocław streams benchmark](wroclaw-streams-benchmark.md)** —
  okay against Flink, Spark, java.util.stream, fs2, zio-streams and
  kyo on one real streaming job: event time, watermarks, keyed
  windows, keyed state. The core-scaling headline, the full table,
  five supporting findings each with its own table, and links to
  every lane's source. Curated from [benchmarks.md](benchmarks.md)
  §20, which carries the full derivation.

## The modules

Each page is the module's full documentation: guide, tutorial,
API reference, gotchas.

| module | what it is |
|---|---|
| `okay` (core) | effects, continuations, the algebra — covered by the guide/tutorial/typepedia above |
| [`okay-async`](modules/okay-async.md) | the portable `Async` effect and its callback-based runtime semantics, with no platform default instances of its own |
| [`okay-direct`](modules/okay-direct.md) | the optional direct syntax (`direct { ... }`) and its compile-time macro implementation |
| [`okay-platform`](modules/okay-platform.md) | the concrete JVM, JavaScript and Native runtimes — `CanBlock`, the schedulers, the net and system facades |
| [`okay-stream`](modules/okay-stream.md) | streams, channels, chunks and the buffers under them; it left the core because nothing in the effect system referred to it, and the core kept only the `Stream` typeclass and `Handoff` |
| [`okay-workflow`](modules/okay-workflow.md) | the static workflow: `Wf`'s questions, `Proc`'s free arrow over them, and the macro that builds one — a leaf of the core, so it became a module |
| [`okay-data`](modules/okay-data.md) | data structures that are not the effect system: the approximate aggregators (`Sketch`), and the coordination-free pair `Uid` and `Hlc`. `Aggregator` itself stayed in the core |
| [`okay-optics`](modules/okay-optics.md) | profunctor optics and the `Fuse` planner: Iso/Lens/Prism/Traversal and friends, the interpretations, and the optic spelling of zooming |
| [`okay-stm`](modules/okay-stm.md) | software transactional memory: the `Tx` language and the runtimes that commit several cells together. The single cell, `TRef`, stayed in the core |
| [`okay-cats`](modules/okay-cats.md) | cats instances (law-tested), IO and free-monad bridges, their runtime as our Scheduler |
| [`okay-zio`](modules/okay-zio.md) | ZIO and ZStream bridges, the ZIO scheduler, ZLayer ⇄ Module |
| [`okay-spring`](modules/okay-spring.md) | a Module as Spring singletons closed with the context, a bean as a module, `A ! Async` from a WebFlux controller (Boot auto-configuration) |
| [`okay-guice`](modules/okay-guice.md) | a Module as Guice bindings by name and type, the closer bound beside them, an injector's instance as a module |
| [`okay-openapi`](modules/okay-openapi.md) | the OpenAPI document as a rendering of the router that serves it; every schema is the codec's own — parameters with the kind their `Param` declared, the body the decoder's, the response the encoder's |
| [`okay-cdi`](modules/okay-cdi.md) | a Module as CDI beans through a portable Extension, the closer at shutdown, a container's instance as a module |
| [`okay-kyo`](modules/okay-kyo.md) | kyo bridges and the structural effect-row mapping |
| [`okay-fs2`](modules/okay-fs2.md) | fs2 streams, chunk for chunk, native backpressure both sides |
| [`okay-kafka`](modules/okay-kafka.md) | Kafka: one poll, one chunk; offsets = the replayable capability |
| [`okay-spark`](modules/okay-spark.md) | Spark via the Aggregator triple — one value, local or distributed |
| [`okay-flink`](modules/okay-flink.md) | Flink via the same triple |
| [`okay-jdbc`](modules/okay-jdbc.md) | JDBC as chunked streams under the Resource region |
| [`okay-r2dbc`](modules/okay-r2dbc.md) | the R2DBC hatch behind the same Sql seam: driver availability on the JVM, honestly framed as not a speed unlock |
| [`okay-delta`](modules/okay-delta.md) | Delta Lake without Spark: Delta Kernel create/append/snapshot/rows over SqlValue rows; DuckDB reads the same table through the JDBC seam |
| [`okay-js`](modules/okay-js.md) | JavaScript as a value: a typed tree, a printer, a compile-time constant, and `js { }` |
| [`okay-ts`](modules/okay-ts.md) | TypeScript programs walked inside okay on Scala.js (multi-shot), and okay handed to TypeScript as a Promise |
| [`okay-ts-npm`](modules/okay-ts-npm.md) | okay as an npm package, `@okay/ts`: programs with async callbacks, CRDT replicas, channels as `AsyncIterable`; its `index.d.ts` written by the module itself |
| [`okay-rust`](modules/okay-rust.md) | Rust kernels over the C ABI through FFM, as an okay effect (`PasswordHash`: Argon2id, byte-equal to BouncyCastle) |
| [`okay-lex`](modules/okay-lex.md) | total streaming tokenization: chunked, snapshottable, incremental |
| [`okay-chain`](modules/okay-chain.md) | blockchains and ledgers read uniformly: CAIP ids, a sans-I/O follower (depth or declared finality, rollbacks said), a ledger projection beside the native transaction |
| [`okay-scalus`](modules/okay-scalus.md) | the Cardano chain as okay-chain events, from a relay over Ouroboros node-to-node, with scalus's ledger model for the blocks |
| [`okay-scalus-spark`](modules/okay-scalus-spark.md) | `spark.read(Stream).format("cardano")`: confirmed Cardano blocks as the same typed tables, a rollback past `confirmations` fails the query |
| [`okay-scalus-flink`](modules/okay-scalus-flink.md) | the Cardano chain as a Flink FLIP-27 source: one split, the last emitted block as its checkpoint, rows from `CardanoTables` through `FlinkSchema` |
| [`okay-x402`](modules/okay-x402.md) | x402: a priced okay-http route answers 402, a paying client pays what its policy allows, a facilitator verifies and settles — only a successful answer is paid for |
| [`okay-x402-evm`](modules/okay-x402-evm.md) | x402's `exact` scheme verified offline: keccak, secp256k1 recovery, EIP-712; the spec's own example signature recovers to its payer |
| [`okay-x402-cdp`](modules/okay-x402-cdp.md) | x402 payments signed by a Coinbase CDP Server Wallet: the typed data goes to CDP (whose policies can refuse), the key stays in its enclave, the signature is recovered before use |
| [`okay-x402-signers`](modules/okay-x402-signers.md) | x402 signers without SDKs: a Circle developer-controlled wallet, Turnkey, or a self-hosted Web3Signer/Clef — typed data signed where the key lives, every answer recovered before use |
| [`okay-x402-mcp`](modules/okay-x402-mcp.md) | x402 over MCP: a priced tool answers JSON-RPC 402 with its price in `error.data`, a paying session pays by policy and asks again, the receipt rides in `result._meta` |
| [`okay-crdt`](modules/okay-crdt.md) | state that merges without a coordinator: the three laws as a runnable check, then GCounter, PNCounter, GSet, OrSet and an Hlc-stamped LwwRegister |
| [`okay-parse`](modules/okay-parse.md) | total lossless parsing; incremental reparse with reference reuse |
| [`okay-codec`](modules/okay-codec.md) | the Schema algebra; JSON, CBOR and Markdown dialects |
| [`okay-llm`](modules/okay-llm.md) | language models as streams; two protocols over one seam; structured output that cuts generation |
| [`okay-agent`](modules/okay-agent.md) | agents as programs: tools as operations, context as a fold, search as Logic |
| [`okay-frame`](modules/okay-frame.md) | what a form is: named slots, typed answers, the language of the exchange, and what is still missing |
| [`okay-intent`](modules/okay-intent.md) | a message to a class and a filled frame, with a model or without one: one Schema is the taxonomy, the frame and the parser — and 73.3% of traffic answered at 88.6% with no network at all |
| [`okay-rag`](modules/okay-rag.md) | retrieval: split the tree not the string, code in eight languages indexed by parsing it, symbols without embeddings |
| [`okay-cluster`](modules/okay-cluster.md) | the remote channel, distributed chunk work, the JS↔JVM acceptance |
| [`okay-http`](modules/okay-http.md) | REST and WebSocket as programs: a body is a `Source`, a socket session is a `Stage[Frame, Frame, A]`, and a socket IS an MCP `Link` |
| [`okay-jetty`](modules/okay-jetty.md) | Jetty behind the same two seams — and the WebSocket SERVER okay-http could not serve |
| [`okay-staging`](modules/okay-staging.md) | the staged codec for a schema that exists only at run time — JVM only, optional by construction, switchable off at launch |
| [`okay-netty`](modules/okay-netty.md) | Netty behind the same two seams, plus the cross-backend matrix that proves the seam |
| [`okay-security`](modules/okay-security.md) | authorization once: claims as values, JWT/JWKS over a crypto seam, PBKDF2, policies as an algebra, routes wrapped so a principal must exist, OAuth2 client flows |
| [`okay-crypto`](modules/okay-crypto.md) | the primitive crypto seam — MAC, hash, KDF, randomness on the platform's own crypto, so okay-pg's SCRAM need not cycle through the security stack |
| [`okay-ui`](modules/okay-ui.md) | the toolkit that is not a toolkit: the view is a value, the renderer is a seam — terminal, React, test host, one application; forms derived from Schema |
| [`okay-ui-gtk`](modules/okay-ui-gtk.md) | GTK 4 on Scala Native over the same Backend seam; aggregated only where pkg-config finds gtk4; one live backend per process, patches marshalled through g_idle_add |
| [`okay-mcp`](modules/okay-mcp.md) | the Model Context Protocol, both ends: a server is a `Handler[Tool]`, our tools are a server, and the protocol is a pure Stage |
| [`okay-persist`](modules/okay-persist.md) | the durable log: one primitive, staged — segments and recovery, offsets, compaction, replication's core, Sql/Kafka store engines, the Doctor |
| [`okay-ops`](modules/okay-ops.md) | health, stats and Prometheus over the persist log's own values: /healthz, /readyz, /stats, /metrics — no SDK, the manifest is the Kubernetes integration |
| [`okay-deploy`](modules/okay-deploy.md) | a whole deployable SYSTEM as one value — services and what each of them needs — rendered to seven targets: a laptop (compose), a rented host (systemd), a cluster (Helm), fly/render/railway, and AWS (Terraform for ECS). Plus the `okay deploy` CLI, which reads `deployment.json` so an artifacts directory works on a server with no repository. [specs/deployment.md](../specs/deployment.md) |
| [`okay-cache`](modules/okay-cache.md) | how a cache is ALLOWED to be wrong, named: budgets, invalidation, the log-fed view; memory and Redis engines; the cross-node invalidation topic |
| [`okay-resilience`](modules/okay-resilience.md) | circuit breaker, bulkhead, keyed token-bucket limiter, hedged requests, a deadline that travels — handlers around one operation, state as values, clock injected |
| [`okay-outbox`](modules/okay-outbox.md) | the log and a database that is ours: the transactional outbox and its relay, the inbox (the unique constraint as idempotency), the dead-letter topic and its replay |
| [`okay-sql`](modules/okay-sql.md) | the relational seam: SqlValue/Col and the typed layer once, drivers underneath (JDBC, the pg wire, sqlite) |
| [`okay-pg`](modules/okay-pg.md) | the Postgres v3 protocol spoken natively: SCRAM (phase objects), the extended protocol, no JVM driver in between |
| [`okay-docs`](modules/okay-docs.md) | the document seam: get/put with CAS, declared consistency; TopicDocs own engine, and Mongo, DynamoDB (SigV4, no SDK) and Cassandra (LWT) on the JVM |
| [`okay-conf`](modules/okay-conf.md) | configuration as data, secrets as REFERENCES — a config cannot leak what it does not contain |
| [`okay-acme`](modules/okay-acme.md) | an ACME (RFC 8555) client: prove the name, get the certificate, renew before it runs out — issue, renew, revoke, external account binding |
| [`okay-tls`](modules/okay-tls.md) | one TLS seam at the transport; the sslmode ladder, verify-full the only default; keys as Secret refs |
| [`okay-mail`](modules/okay-mail.md) | sending mail: SMTP over a socket with STARTTLS, send only, and failure as data |
| [`okay-blob`](modules/okay-blob.md) | the object-store seam: fs and S3 engines, OWN SigV4 pinned by the AWS vectors, persist backups |
| [`okay-obs`](modules/okay-obs.md) | tracing without a framework: spans as values on a topic, W3C traceparent, capability routes, OTLP export as a consumer |
| [`okay-py`](modules/okay-py.md) | Python (and TypeScript, and Haskell) as a handler: typed calls, callbacks into okay's effects, held objects, streams, programs as data (multi-shot), declared environments; a clean-env shim with a version handshake, N workers past the GIL |
| [`okay-r`](modules/okay-r.md) | R as a handler (typed calls, callbacks, held objects, streams, programs as data), the same model with R's own three-way absence: NULL, a TYPED NA inside a vector, and NaN — kept apart at the type level so a statistical function is not quietly handed the wrong one |
| [`okay-script`](modules/okay-script.md) | markdown files as Scala source: fenced blocks through the real Scala 3 compiler in-process, errors pointing at the original `.md` line |
| [`okay-langchain4j`](modules/okay-langchain4j.md) | their ChatModel as a `Handler[Model]` — their provider breadth behind our effect |
| [`okay-langchain4j-embed`](modules/okay-langchain4j-embed.md) | their EmbeddingModel as `String => Embedding` and as okay-rag's `Handler[Embed]` |
| [`okay-onnx`](modules/okay-onnx.md) | the direct ONNX session: the pooled vector AND the token vectors with their characters, from one forward pass — the door `okay.intent.Spans` reads through |
| [`okay-demo-e2e-browser`](modules/okay-demo-e2e-browser.md) | one chat round through a real headless browser — the fetch/ReadableStream glue a JVM test cannot reach |
| [`okay-security-argon2`](modules/okay-security-argon2.md) | the one satellite that buys a dependency: Argon2id in the PHC form, RFC-vector-pinned |
| [`okay-java`](modules/okay-java.md) | the JDK itself as interop: an Aggregator IS a Collector, a Stage IS a Gatherer (JDK 24) |
| [`okay-clojure`](modules/okay-clojure.md) | Clojure from okay, and a Stage IS a Clojure transducer, both ways |
| [`okay-frege`](modules/okay-frege.md) | Frege (a Haskell for the JVM) programs as okay programs — a thin Frege monad over okay's effects; multi-shot, no threads |
| [`okay-scala2`](modules/okay-scala2.md) | okay from Scala 2.13: `Prog`, a fixed-row program type the 2.13 TASTy reader can read, and the two-stdlib classpath it needs |
| [`okay-scala2-codec`](modules/okay-scala2-codec.md) | okay-codec from Scala 2.13: `Schemas.productN`/`sum` in place of `derives Schema`, JSON as text |
| [`okay-scala2-http`](modules/okay-scala2-http.md) | okay-http from Scala 2.13: routes as pattern matching, `Server.use`/`start`, a client |
| [`okay-scala2-sql`](modules/okay-scala2-sql.md) | okay-sql from Scala 2.13: `Db` — rows, statements, verify and transactions as `Eff` |
| [`okay-scala2-agent`](modules/okay-scala2-agent.md) | okay-agent from Scala 2.13: `Chat` with a persistent conversation, `Model`, `Tools`, `Policy` |
| [`okay-scala2-ui`](modules/okay-scala2-ui.md) | okay-ui from Scala 2.13: the Elm loop as an `Eff`, terminal and Swing hosts, a scripted host for tests |
| [`okay-scala2-ws`](modules/okay-scala2-ws.md) | WebSockets from Scala 2.13: a client, sessions as folds (replayable without a socket), a server over okay-jetty |
| [`okay-scala2-resilience`](modules/okay-scala2-resilience.md) | okay-resilience from Scala 2.13: breaker, bulkhead, limiter, hedge, deadline and retry around an `Eff` |
| [`okay-scala2-persist`](modules/okay-scala2-persist.md) | okay-persist for Scala 2.13: topics with defaults, the typed view, streams of records |
| [`okay-scala2-stm`](modules/okay-scala2-stm.md) | okay-stm for Scala 2.13: transactions over TRef with retry and orElse |
| [`okay-scala2-stores`](modules/okay-scala2-stores.md) | okay-cache, okay-blob, okay-docs for Scala 2.13: the stores' operations over Eff and Source |
| [`okay-scala2-llm`](modules/okay-scala2-llm.md) | okay-llm for Scala 2.13: a completion as a token stream, a typed value cut from it |
| [`okay-scala2-rag`](modules/okay-scala2-rag.md) | okay-rag for Scala 2.13: a vector index with the embedder as a plain function |
| [`okay-scala2-mcp`](modules/okay-scala2-mcp.md) | okay-mcp for Scala 2.13: MCP client and server, JSON as text, tools from okay-scala2-agent |
| [`okay-scala2-optics`](modules/okay-scala2-optics.md) | okay-optics for Scala 2.13: lens, prism, affine, traversal, iso as Scala 2 classes over okay's optics |
| [`okay-scala2-workflow`](modules/okay-scala2-workflow.md) | okay-workflow for Scala 2.13: durable programs over a journal, and durable agents |
| [`okay-scala2-services`](modules/okay-scala2-services.md) | okay-actor, -outbox, -obs, -ops, -kafka, -pg for Scala 2.13: the operations that answer programs |
| [`okay-chat`](modules/okay-chat.md) | a streaming LLM chat component: the model seam, Cut-guarded SSE framing, the /chat route — extracted from the demo |
| [`okay-admin`](modules/okay-admin.md) | protected admin routes over the same bearer-token 401/403 ladder as every other protected route |
| [`okay-subscription`](modules/okay-subscription.md) | gate a resource behind a paid period: free for the join month, then paid-this-period or gated, never deleted |
| [`okay-live`](modules/okay-live.md) | broadcast (Hub) and per-key channels (Registry) over the core's own Channel |
| [`okay-demo`](modules/okay-demo.md) | not a library: a coding agent over THIS repository, built from the public surface as a user would (`sbt 'okayDemo/runMain okay.demo.RepoAgent <question>'`); `RepoMcp`, the same repository served as an MCP server (tools, every file as a resource, an `explain` prompt); `ChatDemo`, the chat that runs the stack (streamed replies cut by a guard, a durable task board the model drives through tools, assignments ringing live pages — works with no model); and the worked examples — `Ledger`, the one-binary story (a log, a windowed report over it, a page, a backup that leaves the directory — one process, end to end), and `Combine`, two live telemetry streams joined twice, `Stage.transduce` against fs2's `mapAccumulate` shape, with tests pricing the difference |

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
  The freer monad and extensible-effects design the effect layer is
  built on — and, since 2026-09-15, the ONE tree under both effect
  programs and `Cont` itself: a shift is a freer leaf whose payload
  is a function of the continuation ([theory ch. 11](theory/11-one-tree.md)).
- Robert Atkey —
  [Parameterised notions of computation](https://bentnib.org/paramnotions-jfp.html).
  The parameterised (answer-type-changing) monad `Cont[A, S, R]` is
  founded on.
- Rúnar Óli Bjarnason —
  [Stackless Scala With Free Monads](https://blog.higher-order.com/assets/trampolines.pdf).
  Why stack safety on the JVM means trampolining through data — the
  reason `Free` is a defunctionalized enum with a tail-recursive
  runner rather than raw closures, `Cont` a facade over it, and a
  tail call one `Delay` node.
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
