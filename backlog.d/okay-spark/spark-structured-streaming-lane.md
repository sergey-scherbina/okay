- [ ] spark-structured-streaming-lane — (was `item-172`; named by backlog-audit-0923)
      §20's Spark row is a BATCH job over RDDs: no event time, a window
      is a key, the arrival index carried through the shuffle. Spark's
      answer to an event-time question is Structured Streaming —
      `.withWatermark("ts", "30 seconds").groupBy(window($"ts", "5
      minutes"), $"route")` — and that is the lane that would be
      like-for-like with Flink rather than with `groupingBy`. What it
      needs, all of it known: a `SparkSession` (so the two-stdlib
      classpath hack, which means the lane lives in okay-spark's tests
      and prints its own table), encoders for the row type, a memory or
      rate source with `Trigger.AvailableNow`, a checkpoint directory,
      and reading the result back from the sink. `SparkInterop.toSpark`
      already gives the typed aggregator for the Dataset side, so the
      "one Aggregator, every engine" line holds there too. Trigger: a
      reader who asks what Spark's watermark costs against Flink's.
