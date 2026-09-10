package okay.spark.wroclaw

import okay.wroclaw.Bench

/**
 * Spark's two answers to §20's job: the batch one over RDDs, and its
 * own event-time engine (docs/benchmarks.md §20).
 *
 * THE ORDER MATTERS, and it is not a preference: a JVM may hold ONE
 * SparkContext, the RDD lane opens and closes its own per run, and the
 * streaming lane needs a `SparkSession` for its whole measurement. So
 * the RDD rows finish first and the session is built after them —
 * measured the other way round, the run dies with "Only one
 * SparkContext should be running in this JVM".
 */
object SparkBench {
  def main(args: Array[String]): Unit = Bench.cli(args) { ask =>
    val rdd = Bench.measure(ask, "spark, local[4], batch RDD", 4,
      "no event time: a window is a key")(SparkLane.run(ask.feed, cores = 4))

    val streaming =
      if !ask.wants("structured") then None
      else
        val spark = SparkStreamLane.session(4)
        try
          // the source's CONTENTS, written before anything is timed —
          // a topic's contents are not part of a job's cost either
          val path = SparkStreamLane.stage(spark, ask.feed)
          Bench.measure(ask, "spark, local[4], structured streaming", 4,
            "Spark's own window() and withWatermark")(
            SparkStreamLane.run(spark, path, ask.feed))
        finally spark.stop()

    Seq(rdd, streaming)
  }
}
