# okay-java

Interop with the JDK itself: `java.util.stream` and
`java.util.function` — no dependency to add, it is the platform.

Loads on JDK 17+; `Gather` and `Windowed.gatherer` need JDK 24+ at
RUN time (JEP 485) and are linked lazily, so nothing else here does.

| | |
|---|---|
| `Gather` | an okay `Stage` IS a `java.util.stream.Gatherer` (JDK 24): `Gather.gatherer(stage)` runs a stage in `stream.gather(…)` — a stage that answers short-circuits even an infinite stream — and `Gather.stage(g)` runs any JDK gatherer in an okay pipeline |
| `Collect` | an okay `Aggregator` IS a `java.util.stream.Collector` — the same fold vocabulary both ways, so a JDK stream can finish in okay's aggregators and vice versa |
| `Parallel` | the `Bulk[D[_]]` seam (specs/bulk.md) on a machine's cores: a `java.util.List` is the collection, every step a parallel stream, the aggregation the `Collect` bridge — a platform-free ETL runs here as it runs on Spark |
| `Streams` | `Chunks` <-> `java.util.stream` both ways; the spliterator splits per CHUNK, so the chunk size IS the parallel split size |
| `Windowed` | an EVENT-TIME WINDOW as a `Collector`, and since java-gatherers as a `Gatherer` that pushes each pane downstream the moment the watermark closes it: an `okay.Windows` inside the accumulator, each pane folded into a downstream aggregator as the watermark closes it and evicted — so a JDK stream can window without keeping the whole history |

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

## `Gather`: a stage is a gatherer

A JDK `Gatherer` ([JEP 485](https://openjdk.org/jeps/485)) is the
user-defined INTERMEDIATE operation, as `Collector` is the terminal
one: an initializer, an integrator that pushes any number of elements
and answers `false` to stop, and a finisher. okay's `Stage` is the
same thing written as a program, so the bridge is a translation: the
gatherer's state is the stage suspended at its next `await`, each
element resumes it, each `tell` is a `push`, a stage that answers is
an integrator returning `false`, the finisher resumes it with `None`.
It is law-tested both ways against okay's own run (`TestGather`), and
each of the three claims that matter — short-circuit, a refused push
stopping the stage mid-element, a JDK integrator's `false` stopping
the pulling — has a test that fails without it.

`Windowed.gatherer` is where it earns its place. The collector has to
hand every pane to a fold and answer at the end; the gatherer makes
the panes a stream again:

```scala
val events = Stream.of(Ev(1, "a", 5), Ev(4, "a", 1), Ev(12, "a", 2), Ev(25, "a", 7))

val panes = events
  .gather(Windowed.gatherer[Ev, String, Long, Long](10, 10, 0)(_.key)(_.ts)(sum))
  .toList                                   // [Pane(0,10,a,6), Pane(10,20,a,2), Pane(20,30,a,7)]
```

And the parallel case the collector must refuse is simply right here:
a gatherer with no combiner is evaluated in encounter order even in a
`.parallel()` stream, so the panes are the sequential ones instead of
an exception.

Two limits, both stated in the code. A JDK gatherer's state is an
opaque mutable object that cannot be snapshotted: a pipeline BUILT
with `through(p)(Gather.stage(g))` is a value and runs as often as
you like, each run making its own state, but a continuation from
INSIDE one run resumed again after that run finished is refused by
name. And `Gather` needs JDK 24 to run; okay-java still loads on 17/21,
because a JVM links a class only when it is called.

The P3 doctrine at its cheapest: the platform's own types as
handlers/instances, nothing wrapped that did not need wrapping.
