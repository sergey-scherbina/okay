# okay-java

Interop with the JDK itself: `java.util.stream` and
`java.util.function` — no dependency to add, it is the platform.

| | |
|---|---|
| `Collect` | an okay `Aggregator` IS a `java.util.stream.Collector` — the same fold vocabulary both ways, so a JDK stream can finish in okay's aggregators and vice versa |

The P3 doctrine at its cheapest: the platform's own types as
handlers/instances, nothing wrapped that did not need wrapping.
