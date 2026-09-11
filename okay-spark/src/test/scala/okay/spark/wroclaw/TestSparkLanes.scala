package okay.spark.wroclaw

import okay.wroclaw.{Gtfs, OkayLane}

/**
 * Spark's two lanes answer what okay answers (docs/benchmarks.md §20):
 * the batch RDD rendering, and Structured Streaming with Spark's own
 * `window()` and `withWatermark`.
 */
class TestSparkLanes extends munit.FunSuite {
  override def munitTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(20, "min")
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !Gtfs.present

  test("the batch RDD lane agrees with okay") {
    val feed = Gtfs.events(1)
    assertEquals(SparkLane.run(feed, cores = 4), OkayLane.run(feed))
  }

  test("Structured Streaming, with Spark's own window and watermark, agrees with okay") {
    val feed = Gtfs.events(1)
    val spark = SparkStreamLane.session(4)
    try
      val path = SparkStreamLane.stage(spark, feed)
      assertEquals(SparkStreamLane.run(spark, path, feed), OkayLane.run(feed))
    finally spark.stop()
  }
}
