package okay.lake

import okay.given
import okay.cluster.Flows
import okay.codec.{Json, Schema}
import java.nio.file.{Files, Path}

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
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
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

  test("merge-on-read is refused by name") {
    val dir = Files.createTempDirectory("okay-hudi-mor")
    Files.createDirectories(dir.resolve("t/.hoodie"))
    Files.writeString(dir.resolve("t/.hoodie/hoodie.properties"), "hoodie.table.type=MERGE_ON_READ\nhoodie.table.name=t\n"): Unit
    val lake = s"hudi-mor-${System.nanoTime()}"
    Lakes.register(lake, okay.blob.Fs(dir))
    val e = intercept[IllegalStateException](HudiSource.snapshot(lake, "t"))
    assert(e.getMessage.contains("MERGE_ON_READ"), e.getMessage)
  }
