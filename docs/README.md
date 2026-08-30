# okay — documentation

Okay! Extensible effects for Scala 3, founded on the parameterised
continuation monad. Zero dependencies in the core; JDK 21+ (Loom);
JVM, JS and Native.

- [User guide](guide.md) — the concepts, layer by layer
- [Tutorial](tutorial.md) — from a pure program to a streaming pipeline
- [Typepedia](typepedia.md) — every type and typeclass, with its
  meaning, laws and gotchas
- [Why okay](../README.md#benchmarks-vs-the-ecosystem) — the measured
  comparison tables

## Modules

| module | what it is |
|---|---|
| [`okay`](guide.md) | the core: effects, streams, chunks, the algebra |
| [`okay-cats`](modules/okay-cats.md) | cats instances, IO and free-monad bridges |
| [`okay-zio`](modules/okay-zio.md) | ZIO and ZStream bridges, the ZIO scheduler |
| [`okay-kyo`](modules/okay-kyo.md) | kyo bridges and the structural effect mapping |
| [`okay-fs2`](modules/okay-fs2.md) | fs2 streams, chunk for chunk |
| [`okay-kafka`](modules/okay-kafka.md) | Kafka: one poll, one chunk |
| [`okay-spark`](modules/okay-spark.md) | Spark via the Aggregator triple |
| [`okay-flink`](modules/okay-flink.md) | Flink via the same triple |
| [`okay-jdbc`](modules/okay-jdbc.md) | JDBC as chunked streams under Resource |
| [`okay-lex`](modules/okay-lex.md) | total streaming tokenization, incremental |
| [`okay-parse`](modules/okay-parse.md) | total error-tolerant parsing |
| [`okay-codec`](modules/okay-codec.md) | the Schema algebra and the JSON dialect |
| [`okay-llm`](modules/okay-llm.md) | language models as streams |
| [`okay-cluster`](modules/okay-cluster.md) | the remote channel |

Design documents live in [`specs/`](../specs) — one per feature, with
behavior checkboxes and the decisions (including refuted experiments).
The measured performance history is
[`src/jmh/history.tsv`](../src/jmh/history.tsv).
