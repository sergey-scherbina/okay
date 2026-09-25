package okay2.spark

import okay2.Aggregator
import okay2.stream.Chunks
import SparkInterop._
import org.apache.spark.sql.{Encoders, SparkSession}

/** The same Aggregator value, locally over Chunks and on Spark: equal
 * (the Scala 3 core's own `okay.spark.TestSparkInterop`) — this side
 * needs none of that test's JDK/Security-Manager guard, since it never
 * crosses a Scala version to reach Spark's own Hadoop dependency in a
 * way that changes with the JDK differently than the Scala 3 build. */
class TestSparkInterop extends munit.FunSuite {

  lazy val spark = SparkSession.builder()
    .master("local[2]").appName("okay2-spark-test")
    .config("spark.ui.enabled", "false")
    .getOrCreate()

  override def afterAll(): Unit = spark.stop()

  test("the SAME aggregator: local Chunks run equals the Spark run") {
    val xs = (1 to 10000).map(_.toDouble)
    val agg = Aggregator.variance[Double]
    val local = agg.present(Chunks.fold(Chunks.fromIterator(xs.iterator))(agg.fold))
    val onSpark = aggregate(spark.sparkContext.parallelize(xs, numSlices = 8))(agg)
    assert(math.abs(local - onSpark) < 1e-6, s"local $local vs spark $onSpark")
  }

  test("zip travels too: two statistics, one distributed pass") {
    val rdd = spark.sparkContext.parallelize(1 to 1000, 4)
    val (sum, count) = aggregate(rdd)(Aggregator.sum[Int].zip(Aggregator.count[Int]))
    assertEquals(sum, (1 to 1000).sum)
    assertEquals(count, 1000L)
  }

  test("aggregateByKey: one aggregator per key across partitions") {
    val rdd = spark.sparkContext.parallelize((1 to 100).map(i => (i % 3, i.toLong)), 4)
    val byKey = aggregateByKey(rdd)(Aggregator.sum[Long])
    assertEquals(byKey, (1 to 100).groupBy(_ % 3).map { case (k, v) => (k, v.map(_.toLong).sum) })
  }

  test("the Dataset side: an okay aggregator as a typed column") {
    val ds = spark.createDataset((1 to 500).map(_.toLong))(Encoders.scalaLong)
    val total = aggregate(ds)(Aggregator.sum[Long])(Encoders.scalaLong, Encoders.scalaLong)
    assertEquals(total, (1 to 500).map(_.toLong).sum)
  }
}
