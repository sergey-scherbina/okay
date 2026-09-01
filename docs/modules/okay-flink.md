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

## API reference

| member | signature | meaning |
|---|---|---|
| `FlinkInterop.toFlink` | `Aggregator[In, Acc, Out] => AggregateFunction[In, Acc, Out]` | the whole module |

## Gotchas

- Windowed merges consolidate panes in arbitrary order — assert with
  RELATIVE tolerances on floating-point statistics.
- Serialization of the accumulator is Flink's concern (its
  TypeInformation); primitives and case classes travel fine.
