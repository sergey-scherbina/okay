# okay-spark — one Aggregator, local or distributed

`Aggregator[-In, Acc, +Out]` is `(init, add, merge, present)`. Spark's
`aggregate` is `(zero, seqOp, combOp)`. They are the same triple, so
the SAME VALUE runs over Chunks on a laptop and over an RDD on a
cluster — nothing is adapted, because nothing needs adapting.

`merge` was in the contract from the start precisely so partial
results combine across partition boundaries: that is Spark's combOp,
Flink's merge and okay-cluster's cross-node fold, one definition.

## The pieces

| | |
|---|---|
| `aggregate(rdd)(agg)` | the triple handed straight to `rdd.aggregate` |
| `aggregateByKey(rdd)(agg)` | the same, per key |
| `aggregate(ds)(agg)` | over a `Dataset` |
| `SparkBulk` | the `Bulk` instance: read, map, filter, join, cache and aggregate through the library's own vocabulary rather than Spark's |

## The same aggregator, both ways

```scala
import okay.spark.SparkInterop

val stats = Aggregator.mean[Double].zip(Aggregator.count[Double])

SparkInterop.aggregate(rdd)(stats)   // a cluster
// …and the same `stats` value folds a local Chunks the ordinary way
```

Correctness across splits is not assumed: variance merges by
Chan/Golub/LeVeque, equal to the sequential run up to floating-point
ulps, which is why the tests compare with tolerances and so should
you.

## Further

| | |
|---|---|
| [`docs/modules/okay-spark.md`](../docs/modules/okay-spark.md) | the guide, and what is measured |
| [`specs/aggregators.md`](../specs/aggregators.md) | the algebra the module rides on |
| [`specs/bulk.md`](../specs/bulk.md) | the platform-independent seam `SparkBulk` fills |
