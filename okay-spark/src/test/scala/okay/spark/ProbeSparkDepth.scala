package okay.spark

import org.apache.spark.sql.{Row, SparkSession}
import org.apache.spark.sql.types.*
import scala.jdk.CollectionConverters.*

/**
 * PROBE (spark-deep-values), ignored by default: how deep a struct/array
 * type and a VARIANT value Spark ITSELF survives — createDataFrame,
 * collect, a Parquet write and read back; `parse_json` of a nested
 * array for the variant. The depth SparkSchema refuses at is set from
 * this, below what the first failure here names. Re-run on a new Spark:
 *
 *   un-ignore, then scripts/gate.sh "okaySpark/testOnly okay.spark.ProbeSparkDepth"
 */
class ProbeSparkDepth extends munit.FunSuite:
  override def munitTimeout = scala.concurrent.duration.Duration(20, "min")
  lazy val spark = SparkSession.builder().master("local[2]").appName("probe-depth").config("spark.ui.enabled", "false").getOrCreate()
  override def afterAll(): Unit = spark.stop()

  def nested(depth: Int): (DataType, Any) =
    var t: DataType = LongType
    var v: Any = 7L
    var i = 1
    while i < depth do
      if i % 2 == 0 then { t = ArrayType(t, true); v = Seq(v) }
      else { t = StructType(Seq(StructField("f", t, false))); v = Row(v) }
      i += 1
    (t, v)

  def attempt(label: String)(a: => Unit): Boolean =
    try { a; println(s"  $label ok"); true }
    catch case e: Throwable =>
      val root = Iterator.iterate(e)(_.getCause).takeWhile(_ != null).toSeq.last
      println(s"  $label FAILED: ${root.getClass.getSimpleName} ${Option(root.getMessage).getOrElse("").take(160)}")
      false

  test("the depth Spark survives".ignore) {
    val dir = java.nio.file.Files.createTempDirectory("depth")
    var struct = true
    var variant = true
    for depth <- List(64, 100, 128, 200, 256, 400, 512, 768, 1024, 2048) do
      if struct then
        val (t, v) = nested(depth)
        val st = StructType(Seq(StructField("root", t, false)))
        struct = attempt(s"struct/array depth $depth: createDataFrame + collect") {
          val df = spark.createDataFrame(List(Row(v)).asJava, st)
          assert(df.collect().length == 1)
        } && attempt(s"struct/array depth $depth: parquet write + read") {
          val p = dir.resolve(s"d$depth").toString
          spark.createDataFrame(List(Row(v)).asJava, st).write.parquet(p)
          assert(spark.read.parquet(p).collect().length == 1)
        }
      if variant then
        val json = "[" * depth + "7" + "]" * depth
        variant = attempt(s"variant depth $depth: parse_json + collect") {
          val df = spark.sql(s"select parse_json('$json') as v")
          assert(df.collect().length == 1)
        }
  }
