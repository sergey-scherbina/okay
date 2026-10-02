# okay-flink

> The smallest module in the family, and that is the point: an Okay
> `Aggregator` IS a Flink `AggregateFunction`, field for field.

Depends on: `okay` (JVM), flink-core (pure Java — no cross-version
pain).

## Guide

Flink's `AggregateFunction[In, Acc, Out]` asks for
`createAccumulator / add / merge / getResult`; Okay's `Aggregator`
carries `init / add / merge / present`. `toFlink(agg)` maps one onto
the other with no adaptation — the P1 contract again, third
appearance (Chunks locally, Spark, Flink).

The mapping is exercised the way a WINDOW driver uses it: panes
accumulated separately, merged, presented — equal to the direct run
(with a RELATIVE floating-point tolerance: the Chan/Golub/LeVeque
variance merge drifts more as values grow).

## Tutorial

```scala
import okay.flink.FlinkInterop.toFlink

val fn: AggregateFunction[Double, ?, Double] = toFlink(Aggregator.variance[Double])

stream
  .keyBy(...)
  .window(TumblingEventTimeWindows.of(Time.minutes(5)))
  .aggregate(fn)          // merge handles pane consolidation
```

**Rows from a Schema.** `FlinkSchema` translates okay-codec's
`Columns` — the engine-free tabular reading of any `Schema[A]` — into a
Flink row type and `Row`s: `rowTypeOf[A]`, `rows(xs)`, `rowType(fields)`,
`rowOf(fields, row)`. It decides nothing (the same enum, sum and
recursion encodings Spark gets); a `Json` column is text, Flink having
no VARIANT, and an array is an `OBJECT_ARRAY` of its element's class.

**Flink as a `Bulk`** (bulk-flink; [specs/streams-seam.md](../../specs/streams-seam.md), lane 3).
`FlinkBulk.local(parallelism)` — or `FlinkBulk.FlinkBulk(env)` over an
environment of your own — is a `Bulk` over Flink's DataStream API, run
bounded, so a `Tables` program that names no platform runs on Flink
unchanged, beside `localBulk`, `SparkBulk` and `FlowBulk`. A join is a
`coGroup` keyed on both sides in one end-of-stream window; `aggregate`
is the okay `Aggregator` as Flink's `AggregateFunction`. Elements travel
as `AnyRef` under Flink's generic type information: the Scala 3 macros
that derived a `TypeInformation` per type do not exist, and asking one
per intermediate type is what the seam refuses. `flink-streaming-java`
and `flink-clients` are OPTIONAL dependencies of okay-flink;
`FlinkBulk.missing` names what to add when they are absent, and
`FlinkBulk.local` refuses by that name.

```scala
val p = Tables.of(Vector.range(0, 500)).select(i => (i % 7, i)).join(Tables.of(Vector.tabulate(7)(k => (k, s"k$k"))))
  .collect.map(_.elements.toVector.sorted)
assertEquals(Tables.run(flink)(p), Tables.run(local)(p))
```

## API reference

| member | signature | meaning |
|---|---|---|
| `FlinkInterop.toFlink` | `Aggregator[In, Acc, Out] => AggregateFunction[In, Acc, Out]` | the whole module |

## Measured against the engine itself (2026-09-10)

The mapping had never been run under a real Flink. It is now:
`okay-flink/src/test/scala/okay/flink/wroclaw/` replays Wrocław's GTFS
timetable as an event-time stream (2.4M departures over eight service
days, arrival jitter under the watermark bound) and computes ONE job — tumbling windows per
route, sliding windows per stop, a keyed bunching detector, a per-window
ranking — twice: once through `Chunks` in this JVM, once through a
local Flink MiniCluster, with `Job.stats` the SAME `Aggregator` value
in both. All eleven checksums agree, at parallelism 1 and 4, which is
the module's claim under test end to end (serialization into a job
graph included).

Per event, one okay thread is 4.1x a Flink task and 1.7x four of them,
and four okay fibres — parallel by `Aggregator.merge`, with no shuffle
— are 5.6x four Flink tasks (10.43M ev/s against 1.87M, least squares
over three sizes). Flink pays ~0.4 s of fixed cost per job that okay
does not, and buys with it the checkpointing, recovery and
distribution okay's fold does not offer. A third engine is in the
table too: the same aggregator as a JDK `Collector` over
`java.util.stream`, which has no event time at all and therefore keeps
the whole history. The full table, the methodology and the per-key ordering
trap the run uncovered are in docs/benchmarks.md §20.

Run it: `sbt integrationTest` (the suite is `Live`-tagged and skips
without the GTFS snapshot — `Gtfs`'s scaladoc has the two-line
download), `OKAY_FLINK_DAYS` sets how many service days replay.

## Gotchas

- Windowed merges consolidate panes in arbitrary order — assert with
  RELATIVE tolerances on floating-point statistics.
- Serialization of the accumulator is Flink's concern (its
  TypeInformation); primitives and case classes travel fine.
