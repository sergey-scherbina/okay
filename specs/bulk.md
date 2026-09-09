# Bulk — a collection too large to be in one place, said once

## Overview
The P1 algebra (specs/aggregators.md) made ONE aggregation run locally
over Chunks and on Spark unchanged. What it did not cover is the road
TO the aggregation — reading, projecting, joining, expanding — which
every demo so far wrote twice, once against Spark's API and once against
Scala collections, and which is where a platform's API leaks into a
program. `Bulk[D[_]]` is that road, said once: the handful of things a
data platform can do with a collection it holds, as a typeclass in the
dependency-free core, with an instance per platform in that platform's
interop module. The seam rule of specs/data.md applies: a platform
enters as an INSTANCE of this seam, never as an API surface.

## Interface
```scala
trait Bulk[D[_]]:
  def of[A](xs: Iterable[A]): D[A]              // hand a platform a collection
  def csv(path: String): D[Csv.Row]             // a header-first CSV, named fields
  def map[A, B](d: D[A])(f: A => B): D[B]
  def flatMap[A, B](d: D[A])(f: A => IterableOnce[B]): D[B]
  def filter[A](d: D[A])(p: A => Boolean): D[A]
  def join[K, A, B](l: D[(K, A)], r: D[(K, B)]): D[(K, (A, B))]   // equi-join
  def cache[A](d: D[A]): D[A]                   // materialise: what follows reads it twice
  def aggregate[A, Acc, Out](d: D[A])(agg: Aggregator[A, Acc, Out]): Out
  def toChunks[A](d: D[A]): Chunks[A]          // back to the local world
```
- Instances: `Bulk[Chunks]` (core, scala-jvm reads the files) — one JVM;
  `SparkInterop.bulk(spark): Bulk[SparkInterop.Rows]` — a cluster;
  `Parallel.bulk: Bulk[java.util.List]` (okay-java) — one machine's
  cores through parallel streams and the Collector bridge.
- A program written against `Bulk[D]` names no platform; the platform
  is chosen by the instance in scope. The extension methods make it
  read as a collection (`d.map(f)`, `l.join(r)`, `d.aggregate(agg)`).

## Decisions
- **No per-element evidence.** Spark wants a `ClassTag` for every
  element type and Flink a `TypeInformation`; threading that through a
  generic program means an evidence parameter per intermediate type,
  which is exactly the API leak the seam exists to stop. The Spark
  instance instead keeps every element as `Any` under an opaque wrapper
  (`Rows[A] = RDD[Any]`), which is what Spark stores anyway, and pays
  ONE documented cast at the element boundary — the `Refs.slot`
  precedent. A `ClassTag[Long]` would have unboxed a `collect`; the
  seam collects through `toLocalIterator` and never asks for an array.
- **Replayable by construction.** A `Bulk` value may be consumed more
  than once (the demo aggregates the same departures twice). The local
  instance builds every source under `Chunks.defer`, so a file is
  re-read and an `Iterable` re-iterated per run; `cache` is the explicit
  request to hold rows in memory instead.
- **`csv` is in the seam, not beside it**, because reading is the
  platform's: Spark reads a file in parallel, a JVM reads it in one
  stream, and both hand back the same `Csv.Row`. The local parser is
  RFC 4180 on one line (quotes, doubled quotes, commas inside quotes),
  BOM stripped from the header; quoted newlines are NOT handled and are
  said not to be.
- **The collection view is for code generic in `D`.** On a concrete
  `Chunks[A]` — which is a program, `Chunk[A] ! Produce` — `d.map(f)`
  resolves to the program's own monadic `map`, not the seam's; concrete
  local code calls `B.map(d)(f)`. Generic code (`[D[_]: Bulk]`) reads as
  a collection, and that is the code the seam is for.
- **`join` is the equi-join only.** It is what the demo needs and what
  every platform has natively; anything richer is a program over it.

## Behavior
- [x] the same ETL (Wrocław's GTFS: four CSVs joined, service patterns
      expanded into departures) written once against `Bulk[D]` gives
      equal per-hour aggregates on `Bulk[Chunks]` and on Spark
- [x] `Bulk[java.util.List]`: join and aggregate over parallel streams
      equal the local instance's on the same data
- [x] `Csv.fields` handles quotes, doubled quotes and commas in quotes
- [x] a local source is replayable: aggregating twice reads twice

## Out of scope
- Flink: `flink-core` alone carries no DataStream, so no instance yet;
  the seam's `Any`-element choice is what a `DataStream[AnyRef]`
  instance would do too.
- optimisation across the seam (predicate pushdown, column pruning):
  the seam is the RDD level, not the Catalyst level, on purpose.
