# okay — documentation

Okay! Extensible effects for Scala 3, founded on the parameterised
continuation monad. Zero dependencies in the core; one source for
JVM (JDK 21+, Loom), Scala.js and Scala Native.

## Start here

- **[User guide](guide.md)** — the concepts, layer by layer: control
  → effects → streams → chunks → coroutines → concurrency → the text
  stack → the laziness contract that holds it together.
- **[Tutorial](tutorial.md)** — the same layers by use, twelve
  worked chapters from a pure program to chunks across machines;
  every snippet's shape runs in the repo's tests.
- **[Typepedia](typepedia.md)** — every core type and typeclass with
  its meaning and the recurring gotchas; the reference you grep.
- **[Benchmarks](benchmarks.md)** — every measured case with its
  table, WHY the okay number is what it is, why the competitors'
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
| [`okay-lex`](modules/okay-lex.md) | total streaming tokenization: chunked, snapshottable, incremental |
| [`okay-parse`](modules/okay-parse.md) | total lossless parsing; incremental reparse with reference reuse |
| [`okay-codec`](modules/okay-codec.md) | the Schema algebra; JSON, CBOR and Markdown dialects |
| [`okay-llm`](modules/okay-llm.md) | language models as streams; BPE as a Scan; total structured output |
| [`okay-agent`](modules/okay-agent.md) | agents as programs: tools as operations, context as a fold, search as Logic |
| [`okay-rag`](modules/okay-rag.md) | retrieval: split the tree not the string, code indexed by parsing it, symbols without embeddings |
| [`okay-cluster`](modules/okay-cluster.md) | the remote channel, distributed chunk work, the JS↔JVM acceptance |

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
