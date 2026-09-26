package okay.lake

import okay.given
import okay.arrow.Rows
import okay.blob.Blob
import okay.cluster.Cluster
import okay.parquet.OkayParquet

/**
 * THE LAKE BATTERY (specs/dataflow.md, stage 17), over whatever `Blob`
 * the suite supplies: a directory in the default gate, MinIO Live.
 */
trait LakeSuite extends munit.FunSuite:
  LakeJobs.install()

  /** a fresh lake, registered under the name answered */
  def lake(): String
  /** rows per input object, and input objects */
  def rowsPer: Int = 30000
  def objects: Int = 4

  def blob(name: String): Blob = Lakes(name)

  def trip(i: Long): Trip = Trip(i, s"city ${i % 17}", (i % 1000) / 10.0)

  /** the inputs: `objects` Parquet objects of two row groups each */
  def inputs(name: String): Long =
    for o <- 0 until objects do
      val rows = (0 until rowsPer).map(k => trip(o.toLong * rowsPer + k))
      val bytes = OkayParquet.write(Rows.table(rows), groupRows = (rowsPer + 1) / 2)
      Run(blob(name).putBytes(f"in/trips-$o%03d.parquet", bytes)): Unit
    objects.toLong * rowsPer

  test("a prefix read by 4 workers and written back scored, a worker killed mid-write: every row once, nothing half-visible") {
    val name = lake()
    val total = inputs(name)
    val plan = ParquetSource.plan(name, "in")
    assertEquals(plan.parts.length, objects * 2, "one partition per row group")

    ParquetSource.largest.set(0); ParquetSink.held.set(0)
    Faults.arm(rowsPer.toLong + rowsPer / 2 + 100)       // inside object 1's second group
    val groupRows = 5000
    val got = Cluster.run(ScoreJob, ScoreParams(plan, "out", groupRows), plan.parts.length,
      Vector.fill(4)(Cluster.local)).runWith
    assert(got.failed > 0, "no worker died — the test tested nothing")

    val m = got.value
    assertEquals(m.files.length, plan.parts.length, "one object per partition")
    assertEquals(m.rows, total)
    // the output prefix holds the manifest's objects and nothing else
    val data = Run(okay.Source.concat(blob(name).list("out/_data/"))).map(_.key).toSet
    assertEquals(data, m.files.map(_.key).toSet, "an object the manifest does not name is visible")

    // read back THROUGH the manifest: every trip once, scored
    val back = ParquetSource.plan(name, "out")
    val seen = back.parts.flatMap { p =>
      val size = Run(blob(name).head(p.key)).get.size
      val in = BlobReadAt(blob(name), p.key, size)
      Rows.rows[Scored](OkayParquet.group(in, OkayParquet.footer(in), p.group)).fold(fail(_), identity)
    }
    assertEquals(seen.length.toLong, total)
    assertEquals(seen.map(_.id).distinct.length.toLong, total, "a row written twice")
    assert(seen.forall(s => s.score == ScoreJob.score(trip(s.id))), "a score is wrong")

    // memory: one input group per read, one output group per writer
    val oneGroup = OkayParquet.write(Rows.table((0 until (rowsPer + 1) / 2).map(k => trip(k.toLong))), groupRows = rowsPer).length
    assert(ParquetSource.largest.get <= oneGroup, s"a read of ${ParquetSource.largest.get} bytes; one group is $oneGroup")
    assert(ParquetSink.held.get <= groupRows, s"a writer held ${ParquetSink.held.get} rows")
  }

  test("a stray object under the output prefix is not read, and the commit deletes it") {
    val name = lake()
    val _ = inputs(name)
    val plan = ParquetSource.plan(name, "in")
    // what a lost reply leaves: a whole object nobody will account for
    Run(blob(name).putBytes("out/_data/stray.parquet",
      OkayParquet.write(Rows.table(Vector(Scored(-1, "stray", 0.0)))))): Unit
    Faults.arm(-1)
    val got = Cluster.run(ScoreJob, ScoreParams(plan, "out", 10000), plan.parts.length, Vector.fill(2)(Cluster.local)).runWith
    val back = ParquetSource.plan(name, "out")
    assert(!back.parts.exists(_.key.endsWith("stray.parquet")), "the plan read a stray")
    assertEquals(back.parts.map(_.rows).sum, got.value.rows)
    assertEquals(Run(blob(name).head("out/_data/stray.parquet")), None, "the commit left the stray")
  }

  test("a stream is refused by name: an object is whole only at a partition's end") {
    val name = lake()
    val _ = inputs(name)
    val plan = ParquetSource.plan(name, "in")
    val e = intercept[Throwable](Cluster.stream(ScoreJob, ScoreParams(plan, "out", 1000), plan.parts.length,
      Vector(Cluster.local), 1000).runWith)
    assert(Iterator.iterate(e)(_.getCause).takeWhile(_ != null).exists(_.getMessage.contains("batch run")), e.toString)
  }

  /** where DuckDB finds a lake's objects: `s3://bucket`, or a directory */
  def root(name: String): String
  /** a DuckDB connection that can read `root` */
  def duck(): java.sql.Connection = java.sql.DriverManager.getConnection("jdbc:duckdb:")

  test("DuckDB reads exactly a run's visible output through its manifest, never a stray") {
    val name = lake()
    val _ = inputs(name)
    val plan = ParquetSource.plan(name, "in")
    Faults.arm(-1)
    val got = Cluster.run(ScoreJob, ScoreParams(plan, "out", 10000), plan.parts.length, Vector.fill(2)(Cluster.local)).runWith
    // a writer racing the reader drops an object AFTER the commit: a glob
    // would read it, the manifest does not name it
    Run(blob(name).putBytes("out/_data/late.parquet",
      OkayParquet.write(Rows.table(Vector(Scored(-1, "late", 1e9)))))): Unit
    val m = Manifest.of(name, "out").getOrElse(fail("no manifest"))
    assertEquals(m, got.value)
    val c = duck()
    try
      val rs = c.createStatement().executeQuery(
        s"select count(*), sum(score), count(distinct id) from ${m.duckdb(root(name))}")
      rs.next(): Unit
      assertEquals(rs.getLong(1), got.value.rows)
      assertEquals(rs.getLong(3), got.value.rows, "a row twice")
      val expected = (0L until objects.toLong * rowsPer).map(i => ScoreJob.score(trip(i))).sum
      assertEqualsDouble(rs.getDouble(2), expected, math.abs(expected) * 1e-9)
    finally c.close()
  }

/** the battery over a directory: the default gate */
class TestLake extends LakeSuite:
  private val dirs = scala.collection.mutable.Map.empty[String, java.nio.file.Path]
  def lake(): String =
    val name = s"fs-${System.nanoTime()}"
    val dir = java.nio.file.Files.createTempDirectory("okay-lake")
    dirs.update(name, dir)
    Lakes.register(name, okay.blob.Fs(dir))
    name
  def root(name: String): String = dirs(name).toString
