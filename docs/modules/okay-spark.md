# okay-spark

> One Aggregator, local or distributed: Okay's `(init, add, merge,
> present)` IS Spark's `(zero, seqOp, combOp)` — the same VALUE runs
> over Chunks on a laptop and over an RDD on a cluster.

Depends on: `okay` (JVM), spark-sql (via `CrossVersion.for3Use2_13`).

## Guide

**The contract was designed for this.** P1's `Aggregator[-In, Acc,
+Out]` carries `merge` precisely so partial results combine across
partition boundaries — that is Spark's combOp, Flink's merge, and
okay-cluster's cross-node fold, one definition. `aggregate(rdd)(agg)`
hands the triple straight to `rdd.aggregate`; nothing is adapted,
because nothing needs adapting.

**Correctness across splits.** Variance merges by
Chan/Golub/LeVeque — equal to the sequential run up to floating-point
ulps (the tests use tolerances, and so should you); `zip` computes
several statistics in ONE distributed pass; sketches (HLL, CMS,
t-digest) are aggregators too, so approximate distributed counting
comes for free.

**Datasets too.** `toSpark(agg)` wraps the same value as a
typed-column `sql.expressions.Aggregator` for `Dataset` code;
encoders are the caller's until the Schema-algebra derivation plugs
in (specs/codecs.md — `Schema` folding into `StructType` +
serializers is the stated path).

**Why `Serializable`.** The core's `trait Aggregator extends
Serializable` exists precisely so these closures ship as Spark tasks
— even local mode serializes them.

**The road to the aggregation, said once.** `SparkBulk(spark)` is the
`Bulk[D[_]]` instance (specs/bulk.md): a program that reads CSVs, maps,
joins and expands against `Bulk[D]` names no platform, and runs on
this instance or on the local `Chunks` one unchanged. The instance is
an opaque `RDD[Any]` — Spark stores objects anyway — so no `ClassTag`
is asked per intermediate type; the price is one documented cast at the
element boundary and a boxed element where Spark boxes it too. It is
the RDD level, not Catalyst, and on the Wrocław GTFS job that costs
little: the three joins (1.16M stop times against trips, routes and
calendar, counted) read 708 ms here against 536 ms as a hand-written
DataFrame and 803 ms in one JVM through `Chunks` (MeasureGtfsFrames,
2026-09-30, best of three alternating rounds). The "18 s against 7 s"
this page used to quote was `cache` under Java serialization
(TestWroclawStages), not the RDD level.

**Documents, recognised and routed** (refine-bulk). okay-refine's `Routes`
is written against `Bulk`, so a routing table of document patterns runs
on `SparkBulk` unchanged: `Documents.files` spreads a directory one file
per split, the pattern runs on the executors, each document is
recognised once and cached, and every lane is a filter over that
(TestSparkRoutes: 400 documents, the same lanes, rejects and counts as in
one JVM). See docs/modules/okay-refine.md, "Routes".

**DataFrames in a program, and operators Catalyst can see** (streams-seam,
lane 5; [specs/streams-seam.md](../../specs/streams-seam.md)). `Structured`
(okay-sql) adds two operators a platform can read, beside the opaque
`where(p)` and `join` of `Tables`: `matching(w)` with a `Query.Where` built
from field names checked against `Schema[A]`, and `joinOn(r)(lf, rf)` with
a `Query.Field` on each side. On any platform they run through
`Structured.viaTables`. On Spark, `SparkFrames` brings a DataFrame into the
program (`load[A](df)`, or `read[A](path, "parquet")` pruned to A's fields),
hands a table back (`frame[A](t)`), and keeps a DataFrame-born table in
Catalyst for as long as it meets only structural operators: the predicate
becomes a `Column` (on Parquet a pushed filter at the reader), the join a
DataFrame join. The first opaque step leaves Catalyst, and the same
operators then answer through the RDD with the same result.

```scala
val w = (route === "r1" or route === "r3") and (tram === true)
val got = frames.run(for
  t <- frames.load[Trip](SparkSchema.dataFrame(spark, trips)).plus[Tables + Structured]
  m <- t.matching(w).plus[Tables + State % Tables.Heap[SparkBulk.Rows]]
  df <- frames.frame[Trip](m).plus[Tables + Structured]
  rows <- m.collect.plus[Structured + State % Tables.Heap[SparkBulk.Rows]]
yield (rows.elements.toVector.sortBy(_.id), df.queryExecution.analyzed.toString))
```

**ADTs as DataFrames.** `SparkSchema` turns an okay `Schema[A]` into a
Spark `StructType` and rows — the Catalyst side, where Spark's own
`ExpressionEncoder` (Scala 2 `TypeTag` reflection) sees no Scala 3
enum. It DECIDES nothing: okay-codec's `Columns` makes every tabular
decision engine-free (so the same tables exist without Spark), and
this translates its types and values — a `Json` column becomes a Spark
4 VARIANT (specs/scalus.md §4):

```scala
val df = SparkSchema.dataFrame(spark, Seq(
  Output(1, Credential.KeyHash(Array[Byte](1)), BigInt(2_000_000)),
  Output(2, Credential.ScriptHash(Array[Byte](2)), BigInt(5_000_000))))
// owner: struct<kind: string, KeyHash: struct<hash: binary>, ScriptHash: struct<hash: binary>>
df.createOrReplaceTempView("outputs")
val scripts = spark.sql("SELECT id, lovelace FROM outputs WHERE owner.kind = 'ScriptHash'").collect()
// Array([2,5000000])
```

- a **pure enum** (no case has fields) is a `string` holding the case
  NAME — an ordinal would be renumbered under every old file by the
  next inserted case;
- a **sum with payloads** is a *tagged sparse struct*: `kind` plus one
  nullable struct per case that has fields, exactly one set. A
  field-less case has no branch, because Parquet refuses an empty
  `struct<>`. This is how spark-avro maps a union and spark-protobuf a
  `oneof`, with the discriminator they lack; a new case is a new
  nullable column, so old Parquet files read under the new schema
  (`mergeSchema`), which the tests check;
- a **recursive type** — a named node reachable from itself, found by a
  first fold over the schema graph (mutual recursion included) — is
  `struct<cbor: binary, json: variant>`: okay's CBOR, lossless, and the
  same value as a Spark 4 VARIANT, so `variant_get(t.json, '$.kids[1].label',
  'string')` queries into it. Bounded unrolling was refused: it
  truncates silently;
- `Option` is nullable, `List`/`Vector` an array, `BigInt` a
  `decimal(38,0)` (a uint64 fits; a value past 38 digits is refused).

References: Apache Avro Specification, "Unions"; Protocol Buffers
Language Guide, "Oneof"; Spark SQL `VariantType` (Spark 4.0); Apache Parquet format, nested encoding (definition levels).

## Tutorial

```scala
import okay.spark.SparkInterop.*

val stats = Aggregator.mean[Double].zip(Aggregator.variance[Double])

// locally:
val (m1, v1) = stats.run(data)

// the SAME value, distributed:
val (m2, v2) = aggregate(sc.parallelize(data, 8))(stats)
// per key:
val byKey = aggregateByKey(pairs)(Aggregator.variance[Double])
// typed Dataset column:
ds.select(toSpark(stats).toColumn)

// the road to it, platform-free (okay.Bulk.* is the collection view):
def revenue[D[_]](using B: Bulk[D]) =
  B.csv("sales.csv").map(r => r("shop") -> r("amount").toLong)
    .join(B.csv("shops.csv").map(r => r("id") -> r("city")))
    .aggregate(Aggregator.groupBy((kv: (String, (Long, String))) => kv._2._2)(Aggregator.sum[Long].contramap(_._2._1)))
revenue(using SparkBulk(spark))   // a cluster
revenue(using okay.localBulk)     // one JVM, the same answer
```

## API reference

| member | signature | meaning |
|---|---|---|
| `aggregate` | `(rdd)(agg)(using ClassTag[Acc]) => Out` | rdd.aggregate with the triple |
| `aggregateByKey` | `(rdd)(agg)(using CTs) => RDD[(K, Out)]` | per-key, one pass |
| `toSpark` | `(agg)(using Encoders) => sql.expressions.Aggregator` | the Dataset form |
| `SparkBulk` | `(spark) => Bulk[SparkBulk.Rows]` | the ETL seam on an RDD; `Rows[A]` is an opaque `RDD[Any]` |
| `SparkSchema.structOf` / `rows` / `dataFrame` | `[A](using Schema[A])` | an okay `Schema` as a Spark struct and external rows |
| `SparkSchema.column` | `(Schema[A]) => Col[A]` | one column: type, nullability, value writer |
| `SparkSchema.recursiveNames` | `(Schema[?]) => Set[String]` | the named nodes reachable from themselves |
| `SparkBulk.sort` | `A ! Sort + F => A ! State % Tables.Heap[Rows] + F` | the `Sort` effect answered natively, over the heap `Tables.via` threads |

## Gotchas

- `SparkSchema` refuses a type nested deeper than
  `SparkSchema.MaxNesting` (64 levels) with an `IllegalArgumentException`,
  checked before any conversion starts. That is Arrow's own limit (C++
  `kMaxNestingDepth`), the one okay-arrow keeps. It lets every walk over a
  type recurse without risking the stack. A recursive type is not
  affected: `Columns` folds it into a `json` column first.
- Spark publishes for Scala 2.13 only — the dependency is
  `.cross(CrossVersion.for3Use2_13)`; implicit conversions from the
  Scala 2 API (e.g. `rddToPairRDDFunctions`) must be applied
  EXPLICITLY across the compiler boundary.
- JDK 21 needs forked tests with the `--add-opens` list (13 flags,
  see build.sbt).
- Floating-point merges drift by split point: assert with tolerances,
  relative ones for wide value ranges.
- `SparkBulk(spark)` is a class: the `Bulk` instance and its native
  `sort` together. `csv(path, columns)` selects before `.rdd`, which is
  where a `columns(...)` in the program ends up after the plan rewrite
  (2.5 → 1.3 s on stop_times ⋈ trips); `size(path)` is the file's
  length, the estimate the rewrite orders join sides by.
- `SparkBulk.join` broadcasts a right side of up to `broadcastRows`
  (100 000) rows, found by a bounded `take`; larger ones shuffle. And
  `cache` is serialization: set `spark.serializer` to Kryo, or a persist
  of a few million boxed elements costs more than the build did (the
  Wrocław demo: 18 s with Java serialization, 4.3 s with Kryo).
