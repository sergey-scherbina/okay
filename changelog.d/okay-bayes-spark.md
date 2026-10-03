## okay-bayes-spark - the Bayes model over a Spark RDD

okay-bayes's claim that `observeBulk` runs "where the rows are" had only
been tested on Chunks. TestSparkBayes (okay-spark, which now takes
okay-bayes's tests as `test->test`): the Dark Worlds model, written over
any `Bulk[D]`, over a `SparkBulk` RDD on `local[4]` — its log density and
AD gradient equal the same model's over Chunks at three points to 1e-9, in
the `Smooth` and the `Bayes` form, so the aggregator serialises to the
executors and merges back right; NUTS over the RDD, each gradient one Spark
job, finds the halo where the exact grid does (x 2323.2 vs 2324.1 ± 24.8),
200 draws in 38.7 s. `DarkWorlds.galaxies` takes the sky's directory, since
Spark reads files and not the classpath. Flink is still untested.
