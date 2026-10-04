- [ ] bayes-flink-bulk — operator 2026-10-03. `observeBulk` is proven on
      Chunks and on a Spark RDD (okay-bayes-spark, TestSparkBayes: density
      and AD gradient equal to Chunks' to 1e-9, NUTS finds the halo) but
      NOT on Flink's `Bulk`. The same Dark Worlds model over okay-flink's
      instance, the same two checks. The aggregator must serialise there
      too; Dark Worlds' `start` puts a 108 375-double array in the
      accumulator, worth watching on Flink's network buffers.
