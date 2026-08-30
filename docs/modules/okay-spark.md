# okay-spark

One Aggregator, local or distributed: `(init, add, merge)` IS
`(zero, seqOp, combOp)`.

- `aggregate(rdd)(agg)` / `aggregateByKey(rdd)(agg)` — handed straight
  to Spark; variance across 8 partitions equals the local Chunks run
  (the Chan/Golub/LeVeque merge is the point).
- `toSpark(agg)` — a typed-column `sql.expressions.Aggregator` for
  Datasets; encoders are the caller's until the Schema-algebra
  derivation plugs in (see specs/codecs.md).
- The core's `Aggregator extends Serializable` exists precisely so
  these closures ship as tasks.

Build notes: Spark publishes for Scala 2.13 only —
`CrossVersion.for3Use2_13`; JDK 21 needs forked tests with the
add-opens list (see build.sbt).
