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
the RDD level, not Catalyst: the Wrocław GTFS join reads 18 s here
against 7 s through DataFrames, and 4 s in one JVM through `Chunks`.

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

## Gotchas

- Spark publishes for Scala 2.13 only — the dependency is
  `.cross(CrossVersion.for3Use2_13)`; implicit conversions from the
  Scala 2 API (e.g. `rddToPairRDDFunctions`) must be applied
  EXPLICITLY across the compiler boundary.
- JDK 21 needs forked tests with the `--add-opens` list (13 flags,
  see build.sbt).
- Floating-point merges drift by split point: assert with tolerances,
  relative ones for wide value ranges.
