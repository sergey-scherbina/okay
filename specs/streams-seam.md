# The streams seam — one program, run on Spark, Flink or our engine

## Overview

The operator's ask (2026-09-30): write the logic of a stream job once —
where it reads from, what it does — and run it on Spark, on Flink, on
something else later, or on our own engine; and bring the joins and
zips of specs/stream-join.md into that.

Two seams exist today, and they are not the same thing:

- **`Bulk[D]` + `Tables`** (specs/bulk.md): the BOUNDED program that
  names no platform — `of`, `csv`, `read(path, format)`, `map`,
  `flatMap`, `filter`, `join`, `cache`, `aggregate`, `toChunks`; a plan
  with two measured rewrites; extension by a new signature in the
  effect row (`Sort`). Instances: `Chunks` (one JVM), Spark RDD
  (`SparkBulk`), `java.util.List` (okay-java). Flink has none. Its
  `join` is the hash equi-join alone.
- **`Flow`** (specs/dataflow.md, okay-cluster): the UNBOUNDED,
  PARTITIONED program of our engine — `Src` per partition, `Local`,
  `Owned`, `Keyed`, `Windowed` in event time — with lake and Kafka
  sources. It runs on our engine only.

Nothing carries a sort-merge join, a windowed join or a zip on either
seam: `Chunks.joinSorted`, `Source.joinWithin`, `Chunks.zip` and
`Source.zip` are local operators.

**The decision that shapes the arc:** the platform-free program is the
EFFECT ROW — `Tables`, `Sort`, and the `Streamed` signatures this arc
adds — and `Flow` is one of its targets, exactly as an RDD is. Not the
other way round: `Flow` carries our engine's own decisions (`Finish`,
`Sequential`, watermark seeding), and a Spark or Flink interpreter of
`Flow` would have to fake them. Beam's model (Akidau et al. 2015, "The
Dataflow Model": one pipeline, many runners) is the prior art, and the
honest comparison for the docs page.

## Interface

Lane 1 — `Bulk[Flow]`, our engine as a Bulk instance (okay-cluster):

```scala
package okay.cluster

/** our engine as a platform: `parts` partitions per source, the files
 * read by `lines`, sized by `bytes` (as `Bulk.local` takes them) */
final class FlowBulk(parts: Int,
                     lines: String => Iterator[String] = _ => Iterator.empty,
                     bytes: String => Option[Long] = _ => None)
                    (using Scheduler, CanBlock) extends Bulk[Flow]
```

Lane 2 — `Streamed`, a new signature in the row beside `Tables`/`Sort`
(the extension rule of specs/bulk.md, "a new signature in the row, not a
new method"):

```scala
enum Streamed[+A] derives Effect:
  case JoinSorted[K, A, B](l: Table[(K, A)], r: Table[(K, B)], ord: Ordering[K]) extends Streamed[Table[(K, (A, B))]]
  case JoinWithin[K, A, B](l: Table[(K, A)], r: Table[(K, B)], within: Long, lateness: Long,
                           atL: A => Long, atR: B => Long) extends Streamed[Table[(K, (A, B))]]
  case Windowed[K, A, Acc, O](t: Table[A], size: Long, slide: Long, lateness: Long,
                              key: A => K, at: A => Long, agg: Aggregator[A, Acc, O]) extends Streamed[Table[Pane[K, O]]]
  case Zip[A, B](l: Table[A], r: Table[B]) extends Streamed[Table[(A, B)]]

object Streamed:
  /** the platform-free default: through `toChunks` and the local machines
   * (`SortMerge`, `WindowJoin`, `Windows`, `Chunks.zip`), as `Sort.viaTables` */
  def viaTables[A, G[+_]](p: A ! Streamed + G): A ! Tables + G
```

with a NATIVE answer where a platform has one — Spark's sort-merge join
and window aggregation, Structured Streaming's stream-stream join for
`JoinWithin`; Flink's interval join and windows; our `Flow.Windowed`
and a `Flow.Join` node.

Lane 3 — `Bulk[DataStream]` for Flink, behind an optional dependency,
refused by name without the jar.

Lane 5 — the structural sub-language (Spark DataFrames): `select(fields)`,
`where(pred: okay.sql.Query.Pred)`, join ON a named key field,
`groupBy(field)(agg)`, `sortBy(field)` as plan DATA typed by
`Schema[A]`'s field names, beside the opaque `select(f)` that stays; then
`Bulk[DataFrame]`, honest because a structural plan compiles to column
expressions end to end and Catalyst can see it.

Lane 4 — the docs page: one job, written once, run three ways, with the
numbers.

## Decisions

- **`Bulk[Flow]` runs the engine synchronously, at the seam's boundary.**
  `Bulk`'s `aggregate` answers `Out` and `toChunks` a `Chunks`, plain
  values, so the instance takes a `Scheduler` and a `CanBlock` at
  construction and runs `Flows.fold`/`collect` under them where the seam
  asks for a value. Inside the plan nothing runs: `of`, `map`, `filter`,
  `flatMap` build `Flow` nodes and `read` is `Bulk`'s default — the
  splits spread by `of` (one partition per slice of the split list),
  each read by `flatMap` where it lands, which on this engine is one
  fibre per partition, in parallel.

- **`join` is the seam's hash join, the right side collected once and
  shared by every partition of the left.** The seam's contract
  (specs/bulk.md, "`join` is the equi-join only") is a hash join with the
  right side whole, and `Tables`' rewrite turns the smaller side right
  by size; on this engine the right side is `Flows.collect`ed on first
  demand into one map and the left side streams against it partition by
  partition — Spark's broadcast join, which `SparkBulk` picks for a small
  right side too. The right side is therefore consumed ONCE, not once per
  run, which the seam permits (`cache` promises exactly that). A
  co-partitioned join of two large sides — both sides exchanged by key,
  the join per partition — is a BINARY node `Flow` does not have (every
  node has one `in`); it is lane 2's `Flow.Join`, with the shuffle it
  needs, and the `SortMerge`/`WindowJoin` machines per partition. Not
  lane 1's, and said here so nobody reads this join as distributed.

- **`csv` reads in one partition; `read` in as many as the file has
  splits.** A header-first CSV has no splits (a quoted newline forbids
  cutting it blind), so it is `Flow.one` over `Csv.rows`, deferred per
  run as `Bulk.local`'s is. A Parquet file's row groups are its splits
  and `read` spreads them. Pruning at the parser (`csv(path, columns)`)
  is the local instance's, reused.

- **`cache` and `toChunks` collect through the engine.** `cache` runs the
  flow now and answers `Flow.slices` of the rows (the local instance is
  eager the same way); `toChunks` is DEFERRED — a `Chunks` that runs the
  flow when pulled, so a `Tables` program's `collect` reads the engine's
  answer per run, and a re-read source is re-read.

- **No `Exchange` yet.** Lane 1 adds no node to `Flow`. A `Keyed`
  aggregation is not in `Bulk`'s algebra (its `aggregate` is the whole
  collection's), so the engine's keyed roads (`Finish`, `Sequential`) are
  not reached from `Tables` here; they are reached by `Streamed.Windowed`
  and the `groupBy` of lane 5.

## What a program gains

The day lane 1 lands, a `Tables` program has three backends with no
change to the program: `Tables.run(localBulk)(p)` in one JVM,
`Tables.run(SparkBulk(spark))(p)` on a cluster,
`Tables.run(FlowBulk(parts))(p)` on our engine — `TestPlan`'s job
(two CSVs joined, selected, collected) runs on the third by the
agreement law below.

## Behavior

Lane 1 (streams-seam-1-bulk-flow):

- [x] `FlowBulk(4)` answers what `Bulk.local` answers on the same
      `Tables` program (`TestPlan`'s join job): the same rows
- [x] `of`, `map`, `flatMap`, `filter`, `join`, `aggregate`, `toChunks`
      each agree with the local instance on random input, at parallelism
      1 and 4, and on an empty side
- [x] `read(path, format)` reads every split exactly once, one partition
      per slice of the splits, and `toChunks` consumed twice reads the
      file twice (deferred); `cache` reads it once
- [x] the right side of a join is collected once however many
      partitions the left has
- [x] `csv` and `csv(path, columns)` prune as the local instance does
- [x] docs: docs/modules/okay-cluster.md, "A Bulk over the engine",
      example pinned

Lane 2 (streams-seam-2-streamed): `Streamed` with `viaTables` defaults;
`Flow.Join` (co-partitioned, `SortMerge`/`WindowJoin` per partition,
watermark seeded per partition); Spark's native answers; the agreement
law across Chunks, Spark local, Flow at parallelism 4.

Lane 3 (bulk-flink): `Bulk[DataStream]`, optional, refused by name;
`Streamed` answered natively.

Lane 5 (tables-structural): the structural sub-language, `Bulk[DataFrame]`,
the GTFS job measured three ways — 18 s must approach 7 s.

Lane 4 (streams-seam-docs): the one-job page with the numbers.

## Results

- streams-seam-1-bulk-flow (2026-09-30): ADDITIVE — `FlowBulk` new in
  okay-cluster, two suites, a docs section; no existing body changed.
  Gate: `TestFlowBulk` (5), `TestDocExamplesFlowBulk`, `TestDocSnippets`,
  `affected master Test/compile`. What the tests fixed in the design:
  `runWith` on a `! Async` is the `Effects` instance's extension and
  needs `import okay.given`, not a selective import; `Flow.slices` of an
  empty input at four partitions answers empty (the empty-side law
  passed with no special case); the join's right side is collected on
  the first partition's demand and read exactly once across two
  consumptions (`pulls == 1`), `read` reads each of eight splits once
  per consumption and `cache` once ever — the deferred/eager split of
  the Decisions holds as written.
