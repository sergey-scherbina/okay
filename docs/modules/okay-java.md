# okay-java

Interop with the JDK itself: `java.util.stream` and
`java.util.function` — no dependency to add, it is the platform.

| | |
|---|---|
| `Collect` | an okay `Aggregator` IS a `java.util.stream.Collector` — the same fold vocabulary both ways, so a JDK stream can finish in okay's aggregators and vice versa |
| `Parallel` | the `Bulk[D[_]]` seam (specs/bulk.md) on a machine's cores: a `java.util.List` is the collection, every step a parallel stream, the aggregation the `Collect` bridge — a platform-free ETL runs here as it runs on Spark |
| `Streams` | `Chunks` <-> `java.util.stream` both ways; the spliterator splits per CHUNK, so the chunk size IS the parallel split size |
| `Windowed` | an EVENT-TIME WINDOW as a `Collector`: an `okay.Windows` inside the accumulator, each pane folded into a downstream aggregator as the watermark closes it and evicted — so a JDK stream can window without keeping the whole history |

## `Windowed`: what it fixes, and where it stops

`Collectors.groupingBy` has no notion of a group being COMPLETE, so
windowing a stream with it keeps every pane of the run alive.
docs/benchmarks.md §20 measured the difference on the same job over
Wrocław's timetable: `groupingBy` at 1 160 632 ev/s on 652 MB and
unable to finish 2.4M events at all; `Windowed` at 2 493 921 ev/s on
403 MB, and the full feed in 992 ms. The state model was the
performance problem as much as the memory one.

It is SEQUENTIAL, and its combiner throws rather than being wrong: a
parallel split evicts panes against its own range's watermark, so each
split would report the same window with a partial value and a folded
pane cannot be un-folded. Bounding the state needs a container that
knows it holds a PREFIX of the stream — the coordinator an engine has
and a `Collector` does not. Keyed STATE has no such problem and the
same job shows it: `groupingBy(key, Collect.collector(agg))` holds one
accumulator per key, and the aggregator's `merge` supplies whatever
falls between two halves.

The P3 doctrine at its cheapest: the platform's own types as
handlers/instances, nothing wrapped that did not need wrapping.
