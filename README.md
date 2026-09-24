# Okay! Extensible effects for Scala 3.

There's one thing nearly every effect library does the same way, and I think it's the wrong way.

They hand you one big type: IO, Task, ZIO. Everything is already baked into it — concurrency, errors, environment, cancellation. Want an effect of your own? Either stack monad transformers, or wait for the maintainers to add it to the core.

Okay is built the other way round. An effect is just a set of operations. A handler is an interpretation of them. A program declares in its type what it needs, and knows nothing about how that need will be met.

Here is a real line from the core:

    type Source[W] = Unit ! Writer % W + Async

Read the type out loud: a program that tells W and awaits. That's an asynchronous stream — and it is not a built-in type. It's two ordinary effects, Writer and Async, added together with +, behind a type alias. Every streaming seam in the library speaks it: HTTP response bodies, WebSocket frames, LLM tokens, a chunked source spread across a cluster.

Because the type says only that, and nothing about who provides it, a scripted implementation in a test and a live one over the network are the same type. Nothing mocks anything.

What actually makes this different:

1. The encoding is your choice, not the library author's. The same program lives as a tree (steppable, inspectable, compilable) or as a function (faster). Programs move between the two. Most libraries sell you one and imply the other doesn't exist.

2. Direct style with (almost) no macros. Monadic code reads as ordinary code. That isn't a trick — it's Filinski's 1994 result: given delimited control, any monad runs in direct style. And unlike the Loom-based approach, multi-shot survives here, so nondeterminism and backtracking keep working — on all three platforms.

3. One source for JVM, Scala.js and Scala Native. A platform contributes not API but evidence: "can I park a thread?" So a blocking join in JS code is a compile error, not a runtime failure in front of your user.

4. Zero dependencies in the core. Nothing comes along for the ride.

5. Fast — and measured, not asserted. 10k flatMaps: Okay 5.5 µs against kyo 60, cats IO 163, ZIO 193. A stream pipeline with every lane chunked the same way: 8.2 against fs2 21.9, ZIO 35.8, kyo 65.9 (a bare Iterator is 15.2). Fork/join of 100 fibers: 24 against cats IO 121 — and kyo 18.5, which is a loss, printed as one. Every number has its protocol and its lane rules written down beside it, and a competitor's number is only quoted from the same shape and the same granularity as ours. On a real streaming job across 1/2/4/8 cores, okay scales 5.3x while every other in-process stream library (zio-streams, fs2, kyo, java.util.stream) plateaus around 1.6-1.7x — the full write-up, with source links, is [docs/wroclaw-streams-benchmark.md](docs/wroclaw-streams-benchmark.md) (raw derivation in [docs/benchmarks.md](docs/benchmarks.md), §20).

What that buys you in practice: you don't choose between readable and fast, you don't choose between type-safe and ceremony-free, and you don't need anyone's permission to add an effect of your own. And only you control what every effect (even not yours) actually does in any particular case.

The library stands on work by Moggi, Wadler, Felleisen, Danvy & Filinski, Atkey, Swierstra and Kiselyov — every decision has a paper and a measurement behind it.

Scala 3, just moved to 3.9 LTS. Still on Scala 2.13? `okay-scala2`
gives a 2.13 build effects, several in one program, your own effects,
continuations, streams, fibers and channels, and above them the rest
of the library (HTTP, SQL, agents, UI, the durable log, STM, stores,
retrieval, MCP, optics, durable workflows, actors, Kafka, Postgres),
through types a Scala 2 compiler can read:
[okay from Scala 2.13](docs/scala2.md).
Zero dependencies. One source for JVM (JDK 21+, Loom), Scala.js and
Scala Native — each platform contributes evidence (can it park? what
is its timer? what schedules?), not API: the same Await-based test
suite runs on a JVM, under Node and as a linked native binary.

### Inspired by Rúnar Bjarnason, Oleg Kiselyov and Robert Atkey.

http://blog.higher-order.com/assets/trampolines.pdf
"Stackless Scala With Free Monads" Rúnar Óli Bjarnason

https://okmij.org/ftp/Haskell/extensible/more.pdf
"Freer Monads, More Extensible Effects" Oleg Kiselyov

https://bentnib.org/paramnotions-jfp.html
"Parameterised notions of computation" Robert Atkey

Where every other idea here comes from, with the papers:
[the theory of Okay](docs/theory/index.md).

## Documentation

**[The documentation index](docs/README.md)** is the home page for all
of it — it opens with the same *Start here* list below, then the
modules one line each, how the claims are checked, the design
documents and the papers underneath.

Start here:

| | |
|---|---|
| [User guide](docs/guide.md) | the concepts, layer by layer — control, effects, streams, the upper modules |
| [Continuations: a working book](docs/continuations/index.md) | the long form on the one idea the rest is built from: why a team should care, the four shapes as recipes, the machine, building new effects on it, the costs with numbers, and what it must not be asked to do |
| [Tutorial](docs/tutorial.md) | the same layers by use: worked, runnable examples |
| [Building a chat application](docs/building-a-chat-app.md) | an empty directory to a running streaming chat, outside this repo: depending on an unpublished library, backend, frontend, tests, run |
| [okay from Scala 2.13](docs/scala2.md) | for a Scala 2 codebase: the build, `Eff` with the row as an intersection, your own effect, streams, fibers, and seventeen modules over the rest of the library — every snippet compiled by scalac 2.13 in the gate |
| [okay with TypeScript](docs/typescript.md) | Scala and TypeScript on a backend and a frontend, both in the browser, or both on the backend, with types written once and generated for the other side: a typed `client.ts`, live documents typed by path, `.d.ts` for Scala.js exports, the `@okay/ts` npm package, durable browser flows, a typed Scala facade for TypeScript modules; every generated file checked by `tsc --strict` |
| [Rust and Go as okay](docs/one-language.md) | Rust and Go code taking part in okay's effects as one language: typed operations generated from Scala, direct style `okay_call`, multi-shot programs, over pipes, TCP, FFM and WebAssembly — the same Scala over every link, one conformance suite |
| [okay with Go](docs/go.md) | Go programs on a worker process speaking okay's wire: programs as data, multi-shot continuations, typed operations (`okay.Op[A]`) generated from the Scala callbacks by `Go.ops`, built offline by `GoWorker` |
| [okay with Rust](docs/rust.md) | Rust kernels as okay effects: a Cargo crate over the C ABI, bound through FFM (JDK 22+), mockable and swappable per platform; Argon2id byte-equal to the JVM implementation; Go's roads, and why not in-process |
| [okay with Python and R](docs/python-and-r.md) | Python and R functions as typed Scala functions, their code calling back into okay's effects (`okay.call("name", x)`), held models, modules beside the Scala, streams, declared environments, a journal; each interpreter in its own process |
| [okay with other languages](docs/jvm-languages.md) | Java streams, Clojure and Frege with okay's effects (a stage IS a JDK gatherer and a transducer, programs as data in THAT language); Python and R have [their own page](docs/python-and-r.md); okay runs on JavaScript through Scala.js ([okay-js](docs/modules/okay-js.md)) |
| [Typepedia](docs/typepedia.md) | every core type and typeclass, with its meaning and the recurring gotchas |
| [Capabilities](docs/capabilities.md) | context functions as the wiring: doors, provide, wire — dependency injection with the container deleted |
| [The theory of Okay](docs/theory/index.md) | the textbook: the theories the library stands on, the scientists, the papers, and why each design decision |

Going deeper:

| | |
|---|---|
| [Arrows](docs/arrows.md) | a computation you can see before you run it: the glyphs, two scanners over one input in one pass, and why optics and streams do not meet |
| [Optics](docs/optics.md) | naming a path once: the nested `copy`, the `Option.map` chain and the `case s => s`, each beside the optic that replaces it — with what both cost, and the one pair where the `copy` still wins |
| [Benchmarks](docs/benchmarks.md) | every measured case, why each number is what it is, and where the honest limits are |
| [The cast that could not go](docs/existentials.md) | six encodings tried against one assertion; the five failures are the useful part |
| [Specs](specs/) | the living design documents, one per feature, refutations kept |
| [history.tsv](src/jmh/history.tsv) | the raw measurement log, refuted experiments included |

Per module — every satellite has its own page under
[docs/modules/](docs/modules/): the interop bridges (cats, zio, kyo,
fs2, java, spark, flink, kafka, jdbc), the text stack (lex, parse,
codec), retrieval and agents (rag, llm, agent, mcp), the network
(http, jetty, netty) and the distributed runtime (cluster). The
index above lists them all with one-line summaries.

## Architecture

- `Cont[A, S, R]` (Cont.scala) — the parameterised continuation monad
  (answer-type modification, shift/reset), defunctionalized AS `Free`:
  an opaque `Free[Shift, A]` whose leaf is a function of the
  continuation, so a program and its meaning are one tree, and running
  a flatMap chain is stack-safe ([theory ch. 11](docs/theory/11-one-tree.md)).
- `Control[M[_, _, _]]` (Cont.scala) — final tagless interface of
  delimited control; instances: `Cont` (stack-safe data) and `Func`
  (the function encoding, the reference).
- `Effects[M[_[+_], _]]` (Effects.scala) — final tagless interface of
  extensible effects, founded on the continuation paramonad: a handler
  is `F !> S = F ==> ([X] =>> X /> S)`, an interpretation of the
  operations in Cont, and the meaning of a computation is its `foldCont`;
  `runWith` and `handle` derive from it. Instances: `Free` (initial,
  defunctionalized) and the opt-in `Eager` (pure binds apply at
  construction); `reflect` and `reify` move programs between them.
  For fused build-and-run speed the answer is not another encoding
  but an inline handler-passing program over `Control` (`Interpr`,
  specs/staged-effects.md) — and only when the program is static at
  the call site: on a loop-shaped program the fused `Free` walk wins
  (specs/handler-fusion.md, stage B). The same rule reaches a `direct`
  block: `Direct.staged(Stager.All[E, S, W, Err, A]()) { … }` compiles
  each operation of Reader/State/Writer/Throws to its handler's arm at
  compile time — 2.24x–2.56x over the same block as a Free program,
  parity with the hand-written `Func` program
  (docs/direct-style.md, Layer 2½; docs/benchmarks.md §22).
- `!.relay` (Effects.scala) — tail-resumptive handling: the answer-polymorphic
  handler must resume exactly once, which keeps the loop tail-recursive.
  `Effects.handle` — general handlers (abort, forwarding), via foldCont.

## Effects

- `Reader` — the environment, handled at relay speed (Reader.scala).
- `Writer` — telling IS streaming: a one-constructor GADT
  (`Say(w): Writer[W, Unit]` — a tell answers NOTHING, and matching
  the constructor recovers that, so nothing casts), the element type
  separate from the answer (`A ! Writer % W` computes A telling W);
  run/fold into any Fold algebra, uncons as `Either[A, (W, rest)]`;
  `Writer.of` turns any stream back into the program shape, `Writer.map`
  re-tells at another type (Writer.scala; the five encodings tried
  before this one: docs/existentials.md).
- `State` — get/set with a bespoke tail-recursive handler; `PState` —
  type-changing (typestate) state on the paramonad (State.scala). A
  row holds ONE `State % S`, since `Get()` carries no runtime trace of
  S.
- **Several instances of one effect** — `Tag.Of["small", State % Int]`
  names them in the row, for ANY signature; `tag` puts a finished
  program's operations under a key (so a function written against a
  plain `State % Int` runs twice at two states), and `untag` hands the
  plain signature back to its own handler (Instances.scala). Where the
  instances are made rather than named, `Refs` keeps a heap: cells
  created at run time, one row member however many, one stated cast
  (Refs.scala).
- `Throws` — typed errors: abort, runEither, the `throws` union; and
  `Abort` (= `Throws % Unit`), failure with nothing to say, handled by
  `runOption` (Throws.scala).
- **Your own effect** ([the tutorial](docs/your-own-effect.md)), in
  three lines: `enum Users[+A] derives Effect`
  and the cases carry their answer types. `derives Effect` writes the
  row-split test and registers the signature for direct style;
  `.plus[R]` puts an operation in a wider row; with `Abort` in the row
  a refutable pattern and an `if` guard work in a for-comprehension
  (`case Some(old) <- find(id).plus[Abort]`), so a step that must not run
  is not reachable rather than skipped by hand; `h.tracing(log)`
  makes any handler a recording one (Throws.scala, RowLift.scala,
  docs/guide.md §2, and the worked example in
  `okay-jdbc/src/test/scala/okay/demoeff/UsersDemo.scala`).
- `Choice` — nondeterminism with a genuinely multi-shot handler; the
  canonical MonadPlus (Choice.scala). `Logic` — fair backtracking
  search on top of it: interleave, once, ifte (Logic.scala,
  specs/backtracking.md).
- `Async` — cross-platform: `Run` (a possibly blocking thunk —
  blocking is a JVM/Native ability that parks a virtual thread) and
  `Await` (the universal callback form: an error channel in, a
  canceller out). Blocking is `CanBlock` evidence — absent on JS,
  where `runAsync` drives the same programs through the event loop
  and a blocking join is a compile error. `spawn`/`par`/`race`/
  `timeout`/`sleep` are cross-platform; `Fiber` is
  onComplete/cancel/joinAsync everywhere, parking join under the
  evidence; `Scheduler` takes the program (Loom / the event loop /
  one OS thread per fiber) (Async.scala + Platform.scala per
  platform).
- `Resource` — the region: acquires release at the end of the scope in
  reverse order, surviving handled aborts and mid-step exceptions;
  `bracket` over any Handler-able row (Resource.scala).

## Streams

A stream is codata: one observation, `uncons` — effectful, in
`Stream[S[_], F[+_]]` (F = `Pure` for pure, `Async` for awaited
elements). LazyList is the final coalgebra every stream unfolds into.
Consumption modes, slow-to-fast on a map/filter/take(1000)/sum
pipeline (JMH, us/op, one session; plain Iterator floor = 15.2):

| mode | us | note |
|---|---|---|
| `.toLazyList` + combinators | 172 | memoized, re-observable |
| `.iterator` | 54 | linear, fused, consume-once |
| `Chunks` + `.elements` | 24.5 | chunked source, per-element door |
| `Chunks.map/filter/take`, default chunk of 64 | 10.2 | chunk-in, chunk-out array passes |
| the same, whole input as one chunk | 8.2 | what the competitors' lanes get |
| `Staged` (inline whole-stage) | 1.7 | one fused while-loop |

(On the same pipeline, each library's source chunked the way its own
author intended: fs2 `emits` 21.9, `ZStream.range` 35.8, kyo
`Stream.range` 65.9. Their per-element spellings are 266–1510 and are
not quoted against a chunked lane. `Staged` is the compile-time end of
the choice rule: the `Pipeline` operator tree is for tools — optimize,
inspect, ship — the inline shape is for speed.)

- `Fold`/`Foldable` — the push side; `Monoid` derives folds.
- Writer programs, producers, generators (`generate`/`Put`: one unfold,
  three carriers — LazyList, Producer, Teller) are all streams; effect
  handlers forward the telling, so they are stream transformers.
- `Gen[W]` (Gen.scala) — Python-style generators: a `yield` that
  suspends until read, `for … yield` over a `Gen` with no macro, a
  `generator[W] { … }` block in okay-direct with `Gen.emit`/`Gen.stop`
  and `while`/recursion, `iterator` holding the continuation until the
  next `next()`; the chain of `map`/`filter`/`take`/`flatMap`/`++` is
  fused into the reader — a `map.filter.toList` pipeline reads under
  the hand-written `Writer` road (docs/benchmarks.md §21).
- `Take`/`pipe` (Pipe.scala) — coroutine pipelines: tell meets await
  one element at a time, no channel, no materialization; the consumer
  drives, a finite consumer ends an infinite producer.
  `Stage[I, O, A]` is the transducer as a program; `Stage.transduce`
  is the skeleton they all share (state, a step that tells what the
  input is worth, a flush), `Stage.mapAccumulate` the fs2-shaped 1:1
  special case.
- `Chunks[A] = Feed[Chunk[A]]` (Chunks.scala) — the tree steps per
  chunk, an element costs an array index: generators, transformers,
  zip, rechunk, fold, pipe; spec in specs/chunked-streams.md.

## Concurrency

- `Channel` — the queue between fibers; `merge` combines streams by
  readiness (chunked merge: 13.3 us vs ZIO 51.5 on 2x500), `buffer`
  runs the producer ahead. Parking backpressure on JVM/Native; the
  Await-based JS channel keeps the same surface (Channel.scala per
  platform).
- `Source[W]` — an asynchronous stream as a program
  (`Unit ! Writer % W + Async`), the shape every streaming seam here
  speaks; `a merge b` joins two of DIFFERENT element types into a
  source of their union, bounded by default (an endless source merged
  unbounded measured 1.27M elements produced for 10 consumed).
- Everything runs on virtual threads by default; fork/join of 100
  trivial tasks: 24 us (raw Loom 21.8, kyo 18.5, ZIO 46.6,
  cats-effect 121).
- Across machines: `Remote` ships chunks over a socket into an
  ordinary local Channel, and `Cluster.distribute` spreads a chunked
  source over workers with per-chunk recompute on failure — the
  Aggregator merge is the cross-node contract (okay-cluster).

## Benchmarks vs the ecosystem

JMH, average time in us/op, lower is better. Versions: cats-effect
3.5.7, ZIO 2.1.14, kyo 0.16.2, atnos-eff 7.0.4, fs2 3.10.2. Every
number is from the 2026-09-08 run; the tables, the caveats and the
raw history are in [docs/benchmarks.md](docs/benchmarks.md) and
src/jmh/history.tsv.

Where a competitor appears, both sides are the same SHAPE and the
same granularity. That rule is not decoration: five lanes that broke
it were found on 2026-09-08 alone, and each one read as "we are fast"
or "we are slow" for the wrong reason.

**Bind chain** — 10k left-nested flatMaps, built and run:

| **Okay Eager** | kyo | **Okay Cont** | **Okay Free** | cats Free | cats Eval | cats IO | ZIO | atnos |
|---|---|---|---|---|---|---|---|---|
| **5.5** | 60 | **95** | **112** | 117 | 153 | 163 | 193 | 286 |

(Okay Eager is the kyo trick as an OPT-IN encoding — import Eager.given —
with the hazard stated: construction evaluates, so a self-referential
program diverges before it runs, exactly what compare/TestLaziness
catches kyo on (it runs 513 iterations at the CONSTRUCTION of an
infinite program). Free/Cont keep the laziness contract; the user
chooses per program.)

**Reader** — 10k asks. The shape is part of the measurement: a
for-comprehension nests RIGHT, a `foldLeft` build nests LEFT, and the
starred lanes are O(N²) in that shape.

| shape | **Okay** | kyo Env | ZIO | cats Kleisli | atnos |
|---|---|---|---|---|---|
| right-nested | **79** | 253 | | | |
| left-nested | **116** | 382 800* | 258 | 346 | 1469 |

**Writer** — 10k tells, collected:

| shape | **Okay** | kyo Emit | cats WriterT/Chain | atnos |
|---|---|---|---|---|
| right-nested | **159** | 178 | | |
| left-nested | **217** | 375 400* | 1222 | 3385 |

**Choice** — 2^13 branches, all collected (plain List is the floor):

| List | **Okay** | kyo | atnos |
|---|---|---|---|
| 615 | **1645** | 4185 | 5487 |

**Fork/join** — 100 trivial fibers (raw virtual threads are the floor):

| raw Loom | kyo | **Okay** | ZIO | cats IO |
|---|---|---|---|---|
| 21.8 | **18.5** | 24.0 | 46.6 | 121 |

(A loss, and it is here rather than buried. At 10 000 fibers in each
runtime's own native shape the order reverses — Okay 796, kyo 884 —
docs/benchmarks.md §4b.)

**Stream pipeline** — map/filter/take(1000)/sum, every lane taking the
whole input as ONE chunk (Iterator is the floor):

| Iterator | **Okay Staged** | **Okay chunked** | **Okay chunked, default 64** | fs2 `emits` | **Okay elements** | `ZStream.range` | kyo `Stream.range` |
|---|---|---|---|---|---|---|---|
| 15.2 | **1.70** | **8.22** | **10.21** | 21.9 | **24.5** | 35.8 | 65.9 |

(The default chunk of 64 costs 24% over one chunk, and that is the
honest price of a size a caller gets without asking. Per-element
spellings — kyo singleton 266, `ZStream.iterate` 700, `fs2.iterate`
1510 — are a different question and are in §5, not compared with a
chunked lane here.)

**Merge** — two 500-element streams merged by readiness:

| **Okay chunked** | ZIO | fs2 chunk-native | **Okay elementwise** |
|---|---|---|---|
| **13.3** | 51.5 | 94.4 | **122** |

(fs2 asked fairly: `Stream.emits` a side. Its singleton spelling reads
10 746 in the same run — 114x inside fs2, from the source alone — and
quoting that against a chunked lane is the kind of number this page
stopped printing.)

**Core scaling** — Wrocław's timetable, 2.4M events, event-time
windows and keyed state, one JVM per lane, 1/2/4/8 cores. Full
write-up: [docs/wroclaw-streams-benchmark.md](docs/wroclaw-streams-benchmark.md).
Raw derivation: docs/benchmarks.md §20.

| lane | 1 core | 2 | 4 | 8 | 1→8 |
|---|---|---|---|---|---|
| **Okay** (merge) | 563ms/4.29M | 317ms/7.62M | 189ms/12.77M | **107ms/22.56M** | **5.3x** |
| java.util.stream | 561ms/4.30M | 458ms/5.27M | 396ms/6.10M | 337ms/7.16M | 1.7x |
| zio-streams | 583ms/4.14M | 417ms/5.79M | 357ms/6.76M | 347ms/6.96M | 1.7x |
| kyo | 610ms/3.96M | 512ms/4.72M | 419ms/5.76M | 380ms/6.35M | 1.6x |
| fs2 | 579ms/4.17M | 472ms/5.11M | 377ms/6.40M | 368ms/6.56M | 1.6x |

(wall clock / ev/s. At one core every library is within a few percent — the
measurement's own noise floor, doubled in the same run. What
separates them is the SLOPE: Okay fans the job across fibres joined
by `merge`; the others parallelize one `Stream`/`foreachPar` and hit
its fan-in cost before they run out of cores. Flink sits in the full
table too, at a different scale — a distributed engine's scheduling,
not a library.)

**Resource** — 1000 bracketed acquire/use/release:

| shape | **Okay region** | **Okay bracket** | ZIO | cats IO | kyo |
|---|---|---|---|---|---|
| right-nested | **15.2** | | | | 696 |
| left-nested | **22.5** | **29** | 116 | 225 | 7912* |

**Generators** — the 1000th Fibonacci number, element by element:

| Iterator | LazyList | **Okay Producer** | Okay LazyList | kyo | ZStream | fs2 |
|---|---|---|---|---|---|---|
| 11.7 | 16.2 | **19.2** | 35.5 | 70.7 | 175 | 268 |

Interop: cats, ZIO, kyo, fs2, Kafka, Spark, Flink, JDBC — and the JDK
itself (`okay-java`), where `Aggregator` IS `java.util.stream.Collector`
(supplier/accumulator/combiner/finisher against init/add/merge/present)
and `Chunks` crosses to `Stream` chunk-for-chunk, unboxed in both
directions for `LongStream`/`IntStream`/`DoubleStream`. And Scala 2.13
itself (`okay-scala2`): the same programs behind types that scalac 2.13
reads through `-Ytasty-reader`, tested by a Scala 2.13 suite in the
gate.

## The upper layers

Everything below is built from the primitives above, and each is one
module with its own page under docs/modules:

- **text** — total streaming lex (`okay-lex`, BPE included), total
  lossless parse with O(damage) incremental reparse (`okay-parse`),
  one `Schema` serving JSON/CBOR/Markdown and JSON Schema
  (`okay-codec`).
- **models** — completions as token streams over one transport seam,
  two provider dialects, structured output that cuts generation
  mid-stream (`okay-llm`); retrieval with provenance by construction,
  the index an Aggregator (`okay-rag`); agents as programs — a tool
  call is an effect, context is a fold, policy lives in handlers
  (`okay-agent`).
- **MCP** (`okay-mcp`) — both ends of the Model Context Protocol: a
  server is another `Handler[Tool]`, our tools are another server,
  resources are documents, prompts are conversation openings,
  sampling is the `Model` effect; stdio and streamable HTTP (with
  server push over the GET stream), verified live against the
  protocol's reference server.
- **ui** (`okay-ui`) — the view as a value, the loop as a fold over
  merged sources, the renderer as a seam: one application on a
  terminal, under React, on the raw DOM, in Swing or GTK, over the
  wire to a browser or a native phone client, and on a test host;
  forms derived from the same `Schema` that decodes them — which is
  what lets an MCP server ask the human (elicitation) and get a typed
  answer. A tree carries what its text IS (an identifier is read
  against an explorer, a number is compared down a column) so each
  host sets it in its own idiom, and a client claims the nodes it
  draws natively — a browser draws a real table.
- **security** (`okay-security`) — authorization once: claims as
  values, JWT over a crypto seam, policies as an algebra, protection
  as a route wrapper the type system enforces, OAuth2 client flows —
  zero dependencies, the JDK carries the primitives.
- **wires** — REST and WebSocket as programs (`okay-http`), served by
  the JDK, Jetty or Netty behind one seam (`okay-jetty`,
  `okay-netty`); the distributed runtime (`okay-cluster`).
- **data** — the durable log as one primitive (`okay-persist`): a
  topic of records, compacted snapshots, and on top of them
  **[durable workflows](docs/durable-workflows.md)** — a program that
  waits for a person, a service or a date, written as straight-line
  code and able to outlive the process running it. A paused program is
  a continuation, and a continuation is a closure, so nothing tries to
  write one down: what is journalled is THE ANSWERS, and where the
  program stands is re-derived by running it again over them. That is
  event sourcing with the fold you would otherwise hand-write
  replaced by the program itself. The relational seam (`okay-sql`),
  the lake (`okay-delta`) and the distributed engine (`okay-dataflow`)
  sit beside it.

Building: `sbt test` runs everything — 4736 tests across 93 module
runs, on the JVM, under Node and as a linked native binary (the
live suites — a local model, an npx-spawned MCP server — skip where
their endpoint is absent). Scala 3.9.0 — the LTS line — with 3.6 as
the floor, for the redesigned given syntax, and no ceiling: okay-spark
used to be one, and does not cap the build any more (build.sbt says
what it took) — and sbt 1.13.0 (sbt 2 waits on
sbt-platform-deps, which supplies `%%%` and has no sbt 2 release).
`.jvmopts` gives the build 6g — the launcher's default 4g is shared by
zinc, the compiler and every module at once, and has run out mid-
compile. If you also build in IntelliJ, its Scala compile server has
its own separate 4g cap worth raising for the same reason.

Benchmarks: `sbt "okayJVM/Jmh/run .*FibBenchmark.*"` — the JMH plugin
is on `okay.jvm`, so the project prefix is required (a bare `Jmh/run`
fails to parse, while `Jmh/compile` does not, which is how this line
stayed wrong); comparisons in the
`compare` module (`sbt 'compare/Jmh/run ...'`); history and refuted
experiments in src/jmh/history.tsv.
