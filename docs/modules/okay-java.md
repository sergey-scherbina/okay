# okay-java

Interop with the JDK itself: `java.util.stream` and
`java.util.function` — no dependency to add, it is the platform.

| | |
|---|---|
| `Collect` | an okay `Aggregator` IS a `java.util.stream.Collector` — the same fold vocabulary both ways, so a JDK stream can finish in okay's aggregators and vice versa |
| `Parallel` | the `Bulk[D[_]]` seam (specs/bulk.md) on a machine's cores: a `java.util.List` is the collection, every step a parallel stream, the aggregation the `Collect` bridge — a platform-free ETL runs here as it runs on Spark |

The P3 doctrine at its cheapest: the platform's own types as
handlers/instances, nothing wrapped that did not need wrapping.
