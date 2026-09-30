# One job, written once, run everywhere

A stream or batch job in okay is a PROGRAM that names no platform: a
value of the `Tables` effect (and `Streamed`, `Structured`, `Sort` when it
needs them). The platform is chosen by the `Bulk` instance the program is
run with — the program itself never changes. This page runs one real job
on five of them and prints what each costs.

## The job

The Wrocław public-transport timetable (GTFS, 1,158,821 scheduled stop
departures): every departure joined with its trip's route and service,
whether that route is a tram, and when its service starts — three joins,
then a count. It lives once, in `compare` (`okay.wroclaw.OneJob`):

```scala
  def departures(file: String => String): Long ! Tables =
    val stopTimes = read(file("stop_times.txt")).columns("trip_id", "departure_time")
      .select(r => r("trip_id") -> r("departure_time"))
    val trips = read(file("trips.txt")).columns("trip_id", "route_id", "service_id")
      .select(r => r("trip_id") -> (r("route_id"), r("service_id")))
    val routes = read(file("routes.txt")).columns("route_id", "route_type2_id")
      .select(r => r("route_id") -> (r("route_type2_id") == "31"))
    val calendar = read(file("calendar.txt")).columns("service_id", "start_date")
      .select(r => r("service_id") -> r("start_date"))
    stopTimes.join(trips).select { case (_, (time, (route, service))) => route -> (time, service) }
      .join(routes).select { case (_, ((time, service), tram)) => service -> (time, tram) }
      .join(calendar).aggregate(Aggregator.count[Any])
```

`columns` is structural, so the plan pushes it into the read and each
platform prunes at its own parser; each `join` is chosen by the plan
(`JoinStrategy.Auto`: here a hash join, the smaller side right).

## Five platforms, one program

- one JVM, sequential: `Tables.run(Bulk.local(lines, bytes))(OneJob.departures(file))`
- one JVM, four fibres: `BulkParallel(4, lines, bytes)`
- our cluster engine, four partitions: `FlowBulk(4, lines, bytes)`
- Spark, `local[4]`: `SparkBulk(spark)`
- Flink, a MiniCluster of four: `FlinkBulk.local(4)`

Every platform answered the same 1,158,821 rows. Best of three runs,
2026-10-01, on a shared 14-core machine (load 9–17, so read the ratios,
not the milliseconds):

| platform | time | vs one JVM |
|---|---|---|
| `Bulk.local` | 508 ms | 1.00 |
| `BulkParallel(4)` | 365 ms | 0.72 |
| `FlowBulk(4)` (our engine) | 558 ms | 1.10 |
| `SparkBulk` (Spark local[4]) | 1,647 ms | 3.24 |
| `FlinkBulk` (Flink MiniCluster, 4) | 29,802 ms | 58.7 |

What the numbers say:

- **On one machine, the local instances win.** 1.16M rows fit a JVM, and
  every distributed engine pays for partitions and shuffles it does not
  need here. The engines are for data and machines one JVM does not have;
  the point of the page is that the same program moves there unchanged.
- **Our engine costs 10% over one JVM** at this size: its join exchanges
  both sides by key into buckets, and a join fed by another join passes a
  materialised boundary (the in-process stand-in for a shuffle between
  stages — this page found the chained join failing on the engine, and
  the boundary is how it runs now).
- **Flink is slow through this seam, and why is known.** The seam carries
  elements as `AnyRef` (no per-element evidence — specs/bulk.md), which on
  Flink means generic Kryo serialization of every Scala tuple and map, and
  a join is a `coGroup` in an end-of-stream window that buffers each key's
  groups. A typed road — rows from a `Schema` through `FlinkSchema`, and
  Flink's own join — is what would close most of it.

The measurements are `MeasureOneJob` (compare), `MeasureOneJobSpark`
(okay-spark) and `MeasureOneJobFlink` (okay-flink), `Live`-tagged because
they want the downloaded feed; the row is in `src/jmh/history.d`
(`one-job-everywhere`).

## Literature

Akidau et al., "The Dataflow Model" (VLDB 2015) — one pipeline, many
runners, the prior art for exactly this separation (Apache Beam). Zaharia
et al., "Resilient Distributed Datasets" (NSDI 2012) — the RDD our Spark
instance holds. Carbone et al., "Apache Flink: Stream and Batch
Processing in a Single Engine" (2015).
