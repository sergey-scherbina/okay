- [x] **Flink and Spark: DONE** (2026-09-10,
      bench-engine-native-arithmetic). Flink accumulates through its
      own `AggregateFunction` over POJO accumulators (`StatsAcc`,
      `TopAcc`), Spark's RDD lane through its own `aggregateByKey`
      over a flat `(Long, Long, Int)` and a sort per window for the
      top-5. Both still answer the same eleven checksums. The finding
      the rewrite produced is worth more than the fairness: our
      accumulator was a HANDICAP — `((Long, Long), Option[Int])` is
      unreadable to Flink's type extractor, so its window state went
      through Kryo, and Spark wrote it across every shuffle. `toFlink`
      and `SparkInterop` stay in their own suites, where "one value
      answers on every engine" is a claim about the interop rather
      than a benchmark row.
