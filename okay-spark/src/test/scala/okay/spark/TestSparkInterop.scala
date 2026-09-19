package okay.spark

import okay.{Aggregator, Chunks}
import SparkInterop.*
import org.apache.spark.sql.{Encoders, SparkSession}

/** The same Aggregator value, locally over Chunks and on Spark: equal. */
class TestSparkInterop extends munit.FunSuite {

  /**
   * On JDK 24 the Security Manager is gone (JEP 486), and with it
   * `Subject.getSubject(AccessControlContext)`, which Hadoop's
   * `UserGroupInformation.getCurrentUser` calls on every job:
   *
   *   UnsupportedOperationException: getSubject is not supported
   *
   * There is no flag for it — `-Djava.security.manager=allow` is the
   * remedy for 18..23 and makes a JDK 26 refuse to start at all
   * ("Enabling a Security Manager is not supported") — so 24
   * specifically is skipped rather than left to fail with a stack
   * trace that names Hadoop and explains nothing.
   *
   * 25 is NOT skipped (spark-jdk25-guard-fix, 2026-09-19): Spark
   * 4.2.0 fixed JDK 25 support upstream (SPARK-51167) and this
   * session verified it directly, not just by the JIRA — ran this
   * suite's own SparkSession-creation-and-aggregation path (parallelize,
   * aggregate, compare against the local `Chunks` answer) as a
   * standalone `java -cp <this project's test classpath>` process
   * under `~/.sdkman/candidates/java/25.0.4.1-tem`, bypassing sbt's
   * own test fork entirely: `local=8333333.25 onSpark=8333333.25
   * diff=0.0`, a real distributed job (8 partitions, DAGScheduler,
   * task log) on that JVM. Before this fix the guard read `>= 24`
   * and skipped 25 right along with the genuinely broken 24 — the
   * PREVIOUS session's "sbt's test-fork discovers zero tests under
   * JDK 25, looks like a tooling bug" was this guard doing exactly
   * what it was told, misread as something else. 22 and 23 stay
   * untested — Spark's own 4.2.0 release notes list 17/21/25, not
   * those two, so they stay unknown rather than assumed either way.
   */
  val javaFeature: Int = Runtime.version().feature()
  override def munitIgnore: Boolean = javaFeature == 24

  override def beforeAll(): Unit =
    if munitIgnore then
      println(s"  okay-spark: skipped on Java $javaFeature — JEP 486 removed " +
        "what Hadoop's UGI calls, no workaround exists. " +
        "`sdk env` uses the pinned 21.")

  lazy val spark = SparkSession.builder()
    .master("local[2]").appName("okay-spark-test")
    .config("spark.ui.enabled", "false")
    .getOrCreate()

  override def afterAll(): Unit = spark.stop()

  test("the SAME aggregator: local Chunks run equals the Spark run") {
    val xs = (1 to 10000).map(_.toDouble)
    val agg = Aggregator.variance[Double]
    val local = agg.present(Chunks.fold(Chunks.fromIterator(xs.iterator))(using agg.fold))
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
    assertEquals(byKey, (1 to 100).groupBy(_ % 3).map((k, v) => (k, v.map(_.toLong).sum)))
  }

  test("the Dataset side: an okay aggregator as a typed column") {
    import scala.jdk.CollectionConverters.*
    val ds = spark.createDataset((1 to 500).map(_.toLong).asJava.asScala.toSeq)(using Encoders.scalaLong)
    val total = aggregate(ds)(Aggregator.sum[Long])(using Encoders.scalaLong, Encoders.scalaLong)
    assertEquals(total, (1 to 500).map(_.toLong).sum)
  }
}
