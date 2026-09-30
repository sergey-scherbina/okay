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
  shared by every partition of the left** (lane 1; SUPERSEDED by lane 2:
  `join` is `Flow.Join` with `JoinHow.Hash`, the broadcast road kept as
  `broadcastJoin`). The seam's contract
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

- **`Flow.Join` is the one binary node, a `Wide` over both inputs**
  (lane 2). Its map side is every partition of both inputs — the left's
  first, then the right's — each folding its rows into `buckets` by key
  hash, tagged with its side; its reduce side owns a range of buckets
  and joins each with `JoinHow`: the hash join (the bucket's right rows
  hashed, the left streamed), `Chunks.joinSorted` over the two sides'
  rows in partition order (a globally key-ordered input sliced into
  contiguous partitions stays ordered per bucket; anything else fails
  as unsorted, by name — on the side still being read, since an inner
  join ends at the other side's end), or the interval join with each
  side fed in its time order. The partials are held in memory, as a
  keyed stage's are; a keyed input is refused by name, as a second
  keyed stage is. What this is not: a streaming exchange with
  backpressure between partitions — the engine has none yet, and a
  join that needs one is a lane of its own.

- **`Streamed`'s bounded semantics are the local machines', stated
  once** (lane 2, the class doc of `Streamed`): `JoinWithin` on a table
  is the interval join, each side fed in its time order, so `lateness`
  changes nothing there; `Windowed` keeps the table's order and drops
  as `Windows` drops, which the engine's seeded partitions reproduce at
  any parallelism; `Zip` is positional and goes through `collect` on
  the engine, since two partitioned collections are zippable only when
  partitioned alike. `FlowBulk.streamed` composes as `SparkBulk.sort`
  does — `streamed(Tables.via(B)(p))` — and `FlowBulk.run(p)` is that.

- **No general `Exchange` node.** Lane 1 added none; A `Keyed`
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

Lane 2 (streams-seam-2-streamed):

- [x] `Streamed.viaTables`: `joinSorted` answers the hash join's multiset
      on key-ordered sides; `joinWithin` the interval predicate whatever
      the sides' order, at lateness 0, 5 and 1000 alike; `windowed`
      `Windows`' panes over the table's order with its drops; `zip` is
      `Chunks.zip`
- [x] `Flow.Join`: hash, sort-merge and windowed answer the local law at
      1 and 4 buckets over 1 and 3 partitions a side; an unordered side
      fails by name; a keyed input is refused by name
- [x] a `Tables + Streamed` program answers on `FlowBulk(1)` and
      `FlowBulk(4)` what `viaTables` answers in one JVM — all four
      signatures in one program
- [x] `FlowBulk.join` reads its right side once per run (exchanged);
      `broadcastJoin` once for good
- [x] docs: okay-cluster.md "The stream operators on the engine", pinned
- [ ] Spark's native answers (sort-merge join, windows, Structured
      Streaming's stream-stream join) — a lane of its own, after the
      structural sub-language (lane 5) that makes them Catalyst-visible

Lane 3 (bulk-flink): `Bulk[DataStream]`, optional, refused by name;
`Streamed` answered natively.

Lane 5 (tables-structural, then tables-structural-2): MEASURED FIRST —
the premise "18 s vs 7 s" was stale, the real gap on the GTFS joins is
1.32x (Results) — and BUILT on the operator's reasons, convenience and
compatibility (2026-09-30), not on speed:

- [x] `Structured` (okay-sql): `matching(Query.Where[A])`, `joinOn(r)(lf, rf)`
      answer through `viaTables` what the hand-written filter and key join
      answer, alone and composed with an opaque step — JVM, JS, Native
- [x] `SparkFrames.load` / `frame`: a DataFrame in and out of a program;
      `read(path, "parquet")` pruned to A's fields
- [x] `matching` on a DataFrame-born table is in Catalyst's analyzed
      plan; on Parquet it is a `PushedFilters` entry at the reader
- [x] `joinOn` of two DataFrame-born tables is a DataFrame join and
      answers the local join
- [x] after an opaque step a structural operator answers through the RDD,
      the same answer; `frame` of such a table encodes its rows
- [x] docs: okay-spark.md "DataFrames in a program", example pinned

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

- streams-seam-2-streamed (2026-09-30): NOT additive — `FlowBulk.join`
  changed body (broadcast → `Flow.Join`), `Flows.shape` gained a case;
  gate `affected master staged`. Two test expectations corrected on the
  way, both about semantics worth stating: an inner sort-merge join ENDS
  at the other side's end, so an unordered row after that end is never
  read — the sortedness failure is on the side still being read; and
  an exchanged join reads its right side once per RUN, the broadcast
  road once for good. `Windowed` on the engine at parallelism 4 gave
  `viaTables`' panes including the drops, the seeding claim of
  specs/dataflow.md holding for a table sliced contiguously. Spark's
  native answers deferred: without the structural sub-language a
  Catalyst-side answer would be object mode, which lane 5 is about.

- tables-structural, the measure (2026-09-30): `MeasureGtfsFrames`
  (okay-spark, Live) — the three joins of `Gtfs.departures` without the
  expand, three arms alternating, best of three: our seam on Spark
  (`SparkBulk`, the RDD level, columns pruned at the parser) 708 ms, a
  hand-written DataFrame (`spark.read.csv`, select, three `join` on
  column names, count) 536 ms, `localBulk` in one JVM 803 ms; 1,158,821
  rows in every arm; box load 9-24, so the ratio is the claim, not the
  milliseconds. The "18 s vs 7 s" that this spec, the arc's backlog item
  and docs/modules/okay-spark.md quoted had no DataFrame version behind
  it anywhere in the repository, and TestWroclawStages (bulk-rewrite,
  2026-09-09) had already shown the 18 s was `cache` under Java
  serialization — the record outlived the truth by three weeks and was
  copied forward by this arc. VERDICT for lane 5: on a CSV job Catalyst
  buys 1.32x, which does not earn a second plan language by itself. What
  could: Parquet, where a structural `where` becomes row-group skipping
  and column pruning at the reader (Catalyst's pushdown), which a Scala
  closure can never get — the next measurement before any surface is
  built, on a Parquet table with a selective predicate.

- tables-structural-2 (2026-09-30): `Structured` in okay-sql (the
  predicate is okay-sql's `Query.Where`, which is why it lives there and
  not beside `Tables`: okay-stream cannot see okay-sql), `SparkFrames` in
  okay-spark (okay-spark now depends on okay-sql). Named `matching` and
  `joinOn`, not `where`/`join`: two objects' extensions of one name
  imported together are ambiguous, not overloaded. What the Spark tests
  taught: every closure an executor runs must be free of the session's
  holder (the row codec lives in `object SparkFrames`), a row type
  declared inside a suite ships the suite (`$outer`), `Query.Where` had to
  become `Serializable`, and `Columns.table`'s row function is not
  serializable — `frame` of an RDD-side table encodes value → Json → Row
  by the struct instead. Over rows already in memory Catalyst's
  optimizer evaluates a Filter itself and leaves a `LocalRelation`, so
  the test reads the ANALYZED plan; the Parquet test reads the executed
  plan's `PushedFilters`. Decoding was Row → Json → `Json.decode(Schema)`,
  which lost a `Long` above 2^53 and refused VARIANT and MAP columns —
  SUPERSEDED by spark-values-exact (below).
  `column` walks the predicate with an explicit stack as `Query.eval`
  does; the row walks are bounded by `SparkSchema.MaxNesting`, checked at
  every door. Not additive: gate `affected master staged`.

- spark-values-exact (2026-09-30, operator: "Нужно это исправить"): the
  two named limits of tables-structural-2 removed. `SparkValues` decodes
  by `Schema[A]` straight over Spark's values — no `Json` in between: a
  `Long` at 2^53+1, `Long.MaxValue` and `Long.MinValue`, a 38-digit
  `BigInt` arrive exactly; a recursive type (a 40-level tree) is read
  from the CBOR half of the `(cbor, json)` struct `Columns` writes, at
  the root as well as in a field; a MAP column is read as its entries
  into a sequence of (key, value) products; a VARIANT is read TYPED
  through Spark's `Variant` — an object into a product by key, a long
  exactly, and any value into a `String` field as its JSON text.
  `frame`'s RDD side is encoded by `Columns` on the executors
  (`mapPartitions`, only the schema crossing), which is exact — the
  value → Json → Row road it replaced lost the same longs. Every walk is
  bounded by `SparkSchema.MaxNesting` through a depth parameter. What the
  first run taught: a recursive ROOT is its `(cbor, json)` struct itself,
  with no `value` field around it (`Columns.fields`).

- spark-deep-values (2026-09-30, operator: "Делай"): the nesting bound
  lifted. MEASURED first (`ProbeSparkDepth`, Spark 4.2.0, ignored by
  default): Spark's own stack is not the limit — a 512-level alternating
  struct/array type builds and collects, and the first refusal is
  Jackson's JSON nesting limit (1000) on the SCHEMA's JSON when Parquet
  writes it (at 512 levels; 400 wrote) and on `parse_json`'s input (at
  1024; 768 parsed), both a named `StreamConstraintsException` of
  Spark's. So the 64 SparkSchema refused at was Arrow's limit in a module
  that is not Arrow, and it is gone: `SparkSchema`'s walks (`typeOf`,
  `structAt`, `valueAt`, `rowOf`) and every `SparkValues` walk are
  TRAMPOLINED — a descent is a `!.tailcall` in a program `! Pure`, run by
  `!.run` — and refuse no depth. Proven on a 256 KB stack: a 5000-level
  type and a value of it convert (TestSparkDepth), a 300-level array
  value decodes, a VARIANT 900 levels deep loads into a `String` field
  (TestSparkFrames). Past what Spark itself takes, Spark's own named
  exception is the answer. Arrow's 64 stays okay-arrow's.

- bulk-flink (2026-10-01, lane 3): `FlinkBulk` in okay-flink — a `Bulk`
  over DataStream in BATCH mode; elements as `AnyRef` under generic type
  information; `join` a `coGroup` in `GlobalWindows.createWithEndOfStreamTrigger()`
  (Flink 1.20 has no public `EndOfStreamWindows`), `aggregate` the okay
  `Aggregator` through `AggregateFunction` over the whole stream; the
  functions are top-level classes so the job graph does not capture the
  environment. `flink-streaming-java`/`flink-clients` became
  `optional;test`, refused by name (`FlinkBulk.missing`). Flink 1.20's
  `fromData` over an EMPTY collection generates one record and fails;
  an empty table is a placeholder filtered at once. Tested against the
  local instance on a MiniCluster (`TestFlinkBulk`, Live, as the Wrocław
  Flink lane is). Not yet: `Streamed` answered natively on Flink
  (interval join, event-time windows) — they run through `viaTables`.

- streams-seam-docs (2026-10-01, lane 4): docs/one-job-everywhere.md —
  `okay.wroclaw.OneJob.departures` (compare), the GTFS three joins + count,
  written once and run on `Bulk.local` 508 ms, `BulkParallel(4)` 365,
  `FlowBulk(4)` 558, Spark local[4] 1,647, Flink MiniCluster 29,802 —
  1,158,821 rows on every one (best of 3, load 9-17). The page FOUND a
  defect of lane 2: a join fed by another join failed on the engine
  ("a join's left side follows another keyed stage") and nowhere else;
  `FlowBulk` now passes a keyed side through a lazy materialised
  boundary. Flink's 59x is the generic-Kryo seam plus a windowed coGroup;
  a typed road (FlinkSchema rows, Flink's own join) is the known fix, not
  built. The arc's five lanes are done.

