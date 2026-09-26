package okay.lake

import okay.given
import okay.cluster.Flows
import okay.codec.{Json, Schema}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

final case class Reading(id: Long, city: String, v: Double) derives Schema

/**
 * A HUDI COPY-ON-WRITE TABLE AS A SOURCE (specs/dataflow.md, stage 18),
 * written by HUDI ITSELF — 1.2.1 on Spark 4.1 through pyspark: an insert,
 * an upsert that rewrites every file group (the older slices must not be
 * read), a delete, and an insert_overwrite of one partition (a
 * replacecommit: its old file group must not be read). LIVE: a python
 * with pyspark (`OKAY_PYARROW_PYTHON`), a JDK 21 for Spark
 * (`OKAY_SPARK_JAVA_HOME`), and the Hudi bundle from Maven the first time.
 */
class TestHudi extends munit.FunSuite:
  // the tables are written by Spark: Live; the Avro datum test needs nothing
  override def munitTests(): Seq[Test] =
    super.munitTests().map(t => if t.name.startsWith("a single Avro datum") then t else t.tag(new munit.Tag("Live")))
  override def munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(900, "s")

  val javaHome: Option[String] =
    sys.env.get("OKAY_SPARK_JAVA_HOME").orElse {
      val sdk = Path.of(sys.props("user.home"), ".sdkman/candidates/java")
      if !Files.isDirectory(sdk) then None
      else Files.list(sdk).toArray.map(_.toString).filter(_.matches(".*/21\\.[0-9.]+-tem$")).sorted.lastOption
    }

  lazy val python: Option[String] =
    sys.env.get("OKAY_PYARROW_PYTHON").filter { py =>
      scala.util.Try(ProcessBuilder(py, "-c", "import pyspark").start().waitFor() == 0).getOrElse(false)
    }

  def spark(script: String, args: String*): String =
    val pb = ProcessBuilder((Seq(python.get, "-c", script) ++ args)*).redirectErrorStream(true)
    pb.environment().put("JAVA_HOME", javaHome.get)
    pb.environment().put("PYSPARK_PYTHON", python.get)
    val p = pb.start()
    val said = String(p.getInputStream.readAllBytes())
    if p.waitFor() != 0 then throw IllegalStateException(s"pyspark: ${said.takeRight(3000)}")
    said.linesIterator.filter(_.startsWith("RESULT ")).toVector.lastOption.getOrElse(fail(said.takeRight(3000))).stripPrefix("RESULT ")

  test("Hudi's own COW table — insert, upsert, delete, insert_overwrite — read equal; older slices and a replaced group not read") {
    assume(python.isDefined && javaHome.isDefined, "no pyspark or no JDK 21")
    val dir = Files.createTempDirectory("okay-hudi").toRealPath()
    val said = Json.parse(spark("""
import sys, json, os, glob
from pyspark.sql import SparkSession
d = sys.argv[1]
spark = (SparkSession.builder.master("local[2]")
  .config("spark.jars.packages", "org.apache.hudi:hudi-spark4.1-bundle_2.13:1.2.1")
  .config("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
  .config("spark.sql.extensions", "org.apache.spark.sql.hudi.HoodieSparkSessionExtension")
  .config("spark.sql.catalog.spark_catalog", "org.apache.spark.sql.hudi.catalog.HoodieCatalog")
  .config("spark.ui.enabled", "false")
  .getOrCreate())
path = d + "/readings"
opts = {"hoodie.table.name": "readings", "hoodie.datasource.write.recordkey.field": "id",
        "hoodie.datasource.write.precombine.field": "id", "hoodie.datasource.write.table.type": "COPY_ON_WRITE",
        "hoodie.datasource.write.partitionpath.field": "city", "hoodie.clean.automatic": "false"}
df = lambda lo, hi, m: spark.createDataFrame([(i, "c%d" % (i % 3), i * m) for i in range(lo, hi)], ["id", "city", "v"])
df(0, 1000, 1.0).write.format("hudi").options(**opts).mode("overwrite").save(path)
df(500, 700, 10.0).write.format("hudi").options(**opts).option("hoodie.datasource.write.operation", "upsert").mode("append").save(path)
df(0, 60, 1.0).write.format("hudi").options(**opts).option("hoodie.datasource.write.operation", "delete").mode("append").save(path)
spark.createDataFrame([(5000 + i * 3 + 2, "c2", 7.0) for i in range(20)], ["id", "city", "v"]) \
  .write.format("hudi").options(**opts).option("hoodie.datasource.write.operation", "insert_overwrite").mode("append").save(path)
r = spark.read.format("hudi").load(path)
row = r.agg({"v": "sum", "id": "sum"}).collect()[0]
files = sorted(os.path.relpath(f, d) for f in glob.glob(path + "/*/*.parquet"))
print("RESULT " + json.dumps({"rows": r.count(), "v": row["sum(v)"], "ids": row["sum(id)"], "files": files}))
""", dir.toString))
    def f(k: String) = said match { case Json.JObj(fs) => fs.collectFirst { case (`k`, v) => v }.get; case _ => fail(said.toString) }
    def num(j: Json) = j match { case Json.JNum(n) => n; case _ => fail(j.toString) }
    val lake = s"hudi-${System.nanoTime()}"
    Lakes.register(lake, okay.blob.Fs(dir))
    val snap = HudiSource.snapshot(lake, "readings")
    assertEquals(snap.layout, 2, "Hudi 1.x writes timeline layout 2")
    val all = f("files") match { case Json.JArr(vs) => vs.collect { case Json.JStr(s) => s }; case _ => Vector() }
    val planned = snap.files.map(_._1).toSet
    val skipped = all.filterNot(planned)
    assert(skipped.length >= 3, s"only ${skipped.length} files left unplanned of ${all.length}: no older slice or replaced group was tested")
    assert(!planned.exists(_.startsWith("readings/c2/")) || snap.files.count(_._1.startsWith("readings/c2/")) == 1,
      s"a replaced c2 file group is planned: ${snap.files}")
    val rows = Flows.collect(ParquetSource.flow[Reading](HudiSource.plan(lake, "readings"))).runWith
    assertEquals(rows.length.toLong, num(f("rows")).toLong)
    assertEqualsDouble(rows.map(_.v).sum, num(f("v")), 1e-6)
    assertEquals(rows.map(_.id).sum, num(f("ids")).toLong)
    assertEquals(rows.map(_.id).distinct.length, rows.length, "a record read twice: an older slice was planned")
  }

  test("Hudi's own MERGE_ON_READ table — insert, upsert, an older event's upsert, delete — read equal; an uncommitted instant's blocks skipped") {
    assume(python.isDefined && javaHome.isDefined, "no pyspark or no JDK 21")
    val dir = Files.createTempDirectory("okay-hudi-mor").toRealPath()
    val said = Json.parse(spark("""
import sys, json
from pyspark.sql import SparkSession
d = sys.argv[1]
spark = (SparkSession.builder.master("local[2]")
  .config("spark.jars.packages", "org.apache.hudi:hudi-spark4.1-bundle_2.13:1.2.1")
  .config("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
  .config("spark.sql.extensions", "org.apache.spark.sql.hudi.HoodieSparkSessionExtension")
  .config("spark.sql.catalog.spark_catalog", "org.apache.spark.sql.hudi.catalog.HoodieCatalog")
  .config("spark.ui.enabled", "false")
  .getOrCreate())
path = d + "/readings"
opts = {"hoodie.table.name": "readings", "hoodie.datasource.write.recordkey.field": "id",
        "hoodie.datasource.write.precombine.field": "ts", "hoodie.datasource.write.table.type": "MERGE_ON_READ",
        "hoodie.datasource.write.partitionpath.field": "city", "hoodie.clean.automatic": "false",
        "hoodie.compact.inline": "false"}
df = lambda lo, hi, m, ts: spark.createDataFrame([(i, "c%d" % (i % 3), i * m, ts) for i in range(lo, hi)], ["id", "city", "v", "ts"])
up = lambda d, op: d.write.format("hudi").options(**opts).option("hoodie.datasource.write.operation", op).mode("append").save(path)
df(0, 300, 1.0, 1).write.format("hudi").options(**opts).mode("overwrite").save(path)
up(df(100, 150, 10.0, 2), "upsert")
up(df(140, 160, -1.0, 0), "upsert")
agg = lambda: spark.read.format("hudi").load(path).agg({"v": "sum", "id": "sum", "*": "count"}).collect()[0]
before = agg()
up(df(0, 20, 1.0, 1), "delete")
after = agg()
res = lambda r: {"rows": r["count(1)"], "v": r["sum(v)"], "ids": r["sum(id)"]}
print("RESULT " + json.dumps({"before": res(before), "after": res(after)}))
""", dir.toString))
    def f(j: Json, k: String) = j match { case Json.JObj(fs) => fs.collectFirst { case (`k`, v) => v }.get; case _ => fail(j.toString) }
    def num(j: Json) = j match { case Json.JNum(n) => n; case _ => fail(j.toString) }
    val lake = s"hudi-mor-${System.nanoTime()}"
    Lakes.register(lake, okay.blob.Fs(dir))
    def check(expected: Json) =
      val plan = HudiSource.plan(lake, "readings")
      assert(plan.parts.exists(p => p.group == -1 && p.logs.nonEmpty), s"no slice with logs: ${plan.parts}")
      val rows = Flows.collect(ParquetSource.flow[Reading](plan)).runWith
      assertEquals(rows.length.toLong, num(f(expected, "rows")).toLong)
      assertEqualsDouble(rows.map(_.v).sum, num(f(expected, "v")), 1e-6)
      assertEquals(rows.map(_.id).sum, num(f(expected, "ids")).toLong)
      assertEquals(rows.map(_.id).distinct.length, rows.length, "a record read twice")
    check(f(said, "after"))

    // the delete's instant, uncommitted: its blocks stay in the log and must not count
    val timeline = dir.resolve("readings/.hoodie/timeline")
    val deltas = Files.list(timeline).toList.asScala.toVector.filter(_.getFileName.toString.matches("\\d+_\\d+\\.deltacommit")).sortBy(_.toString)
    val last = deltas.last
    val aside = dir.resolve(last.getFileName.toString)
    Files.move(last, aside): Unit
    check(f(said, "before"))
    Files.move(aside, last): Unit

    // a data block this reader does not decode is refused by name, not skipped
    val log = Files.walk(dir.resolve("readings")).toList.asScala.toVector
      .filter(p => !p.toString.contains("/.hoodie/") && p.getFileName.toString.matches("\\.[^.].*\\.log\\.\\d+_[^.]+"))
      .sortBy(_.toString).head   // a data log — not the metadata table's under .hoodie/
    val bytes = Files.readAllBytes(log)
    java.nio.ByteBuffer.wrap(bytes).putInt(6 + 8 + 4, 5): Unit
    Files.write(log, bytes): Unit
    val e = intercept[Exception](Flows.collect(ParquetSource.flow[Reading](HudiSource.plan(lake, "readings"))).runWith)
    assert(Iterator.iterate[Throwable](e)(_.getCause).takeWhile(_ != null).exists(x => String.valueOf(x.getMessage).contains("PARQUET_DATA")), e.toString)
  }

  test("a single Avro datum (how a log block holds its records): ours and Apache Avro's decoders read Apache's writer the same") {
    val schema = """{"type":"record","name":"R","fields":[{"name":"id","type":"long"},{"name":"city","type":["null","string"]},""" +
      """{"name":"v","type":"double"},{"name":"tags","type":{"type":"array","items":"string"}},{"name":"raw","type":"bytes"},""" +
      """{"name":"inner","type":["null",{"type":"record","name":"I","fields":[{"name":"n","type":"int"}]}]}]}"""
    val s = org.apache.avro.Schema.Parser().parse(schema)
    val r = org.apache.avro.generic.GenericData.Record(s)
    r.put("id", 42L); r.put("city", "c1"); r.put("v", 2.5); r.put("tags", java.util.List.of("a", "b"))
    r.put("raw", java.nio.ByteBuffer.wrap(Array[Byte](1, 2, 3)))
    val inner = org.apache.avro.generic.GenericData.Record(s.getField("inner").schema.getTypes.get(1))
    inner.put("n", 7)
    r.put("inner", inner)
    val out = java.io.ByteArrayOutputStream()
    val enc = org.apache.avro.io.EncoderFactory.get().binaryEncoder(out, null)
    org.apache.avro.generic.GenericDatumWriter[org.apache.avro.generic.GenericRecord](s).write(r, enc)
    enc.flush()
    def plain(v: Any): Any = v match
      case b: Array[Byte] => b.toVector
      case xs: Vector[?] => xs.map(plain)
      case (k, x) => (k, plain(x))
      case other => other
    val ours = plain(OkayAvro.decoder(schema)(out.toByteArray))
    assertEquals(ours, plain(ApacheAvro.decoder(schema)(out.toByteArray)))
    assertEquals(ours, Vector("id" -> 42L, "city" -> "c1", "v" -> 2.5, "tags" -> Vector("a", "b"),
      "raw" -> Vector[Byte](1, 2, 3), "inner" -> Vector("n" -> 7L)))
  }
