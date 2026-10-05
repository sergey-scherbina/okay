package okay.spark

import java.nio.charset.StandardCharsets.UTF_8
import org.apache.spark.sql.SparkSession

class TestSparkEvidence extends okay.testkit.Munit.Diagnosed:
  private lazy val spark = SparkSession.builder().master("local[2]").appName("spark-evidence")
    .config("spark.ui.enabled", "false").getOrCreate()
  override def afterAll(): Unit = spark.stop()
  private val canonical = new SparkEvidence.Canonical[String]:
    def bytes(value: String): Array[Byte] = value.getBytes(UTF_8)
  private def receipt(m: SparkEvidence.Manifest): SparkEvidence.Receipt =
    SparkEvidence.Receipt("memory://" + m.runId, m.digest)

  test("real Spark counts partitions and preserves logical digest across runs") {
    val rdd = spark.sparkContext.parallelize(Vector("one", "two", "three"), 4)
    val first = SparkEvidence.run(rdd, "run")(canonical)(receipt)
    val second = SparkEvidence.run(rdd, "run")(canonical)(receipt)
    note(s"first=${first.manifest}, second=${second.manifest}")
    assertEquals(first.manifest.partitions.map(_.id), Vector(0, 1, 2, 3))
    assertEquals(first.manifest.partitions.map(_.count).sum, 3L)
    assertEquals(first.manifest.digest, second.manifest.digest)
  }
  test("length framing distinguishes concatenations and sink failure propagates") {
    def run(xs: Vector[String]) = SparkEvidence.run(spark.sparkContext.parallelize(xs, 1), "run")(canonical)(receipt)
    assertNotEquals(run(Vector("ab", "c")).manifest.digest, run(Vector("a", "bc")).manifest.digest)
    intercept[IllegalStateException] {
      SparkEvidence.run(spark.sparkContext.parallelize(Vector("x"), 1), "run")(canonical)(_ => throw new IllegalStateException("sink failed"))
    }
  }
