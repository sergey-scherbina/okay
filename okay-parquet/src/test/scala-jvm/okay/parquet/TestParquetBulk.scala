package okay.parquet

import okay.localBulk
import okay.freer.Aggregator
import okay.Chunks.elements
import okay.arrow.{Column, Rows, Table, TimeUnit}
import okay.codec.Schema
import java.nio.file.Files
import java.util.concurrent.atomic.AtomicLong

final case class Reading(id: Long, city: String, v: Double) derives Schema
final case class Slim(at: Long, id: Long) derives Schema

/** a Parquet file as a `Bulk` source, no Spark (bulk-parquet, specs/bulk.md) */
class TestParquetBulk extends munit.FunSuite:

  val n = 10000
  val readings: Vector[Reading] = Vector.tabulate(n)(i => Reading(i.toLong, s"c${i % 3}", i * 0.5))
  // the rows, a timestamp column (microseconds) and a wide column no reader here
  // names — random, so no page compression hides what reading it costs
  val table: Table =
    val t = Rows.table(readings)
    Table(t.cols ++ Vector(
      "at" -> Column.Timestamp(TimeUnit.Micro, Some("UTC"), Array.tabulate(n)(i => 1704067200000000L + i * 1000000L), Array.fill(n)(true)),
      "wide" -> { val rnd = scala.util.Random(7); Column.Utf8(Array.fill(n)(rnd.alphanumeric.take(200).mkString), Array.fill(n)(true)) }), t.metadata)

  val file: String =
    val f = Files.createTempFile("okay-bulk", ".parquet")
    Files.write(f, OkayParquet.write(table, groupRows = 3000)): Unit
    f.toString

  test("Bulk.read on localBulk: a split per row group, the rows as written, replayable") {
    val format = ParquetFile.rows[Reading]
    assertEquals(format.splits(file), Vector(0, 1, 2, 3))
    val d = localBulk.read(file, format)
    assertEquals(d.elements.toVector, readings)
    val sum = Aggregator.sum[Double].contramap[Reading](_.v)
    val once = localBulk.aggregate(d)(sum)
    assertEquals(once, readings.map(_.v).sum)
    assertEquals(localBulk.aggregate(d)(sum), once, "a second run reads the file again")
  }

  test("a timestamp column lands in a Long field, its raw microseconds") {
    val got = localBulk.read(file, ParquetFile.rows[Slim]).elements.toVector
    assertEquals(got.length, n)
    assertEquals(got(7), Slim(1704067200000000L + 7000000L, 7L))
  }

  test("pruned at the reader: a type naming two columns reads a fraction of the bytes") {
    def bytes[A](using Schema[A]): Long =
      val seen = AtomicLong(0)
      val open: String => ReadAt = p =>
        val in = ParquetFile.readAt(p)
        new ReadAt:
          def size: Long = in.size
          def read(offset: Long, len: Int): Array[Byte] = { seen.addAndGet(len.toLong): Unit; in.read(offset, len) }
      val format = ParquetFormat[A](open)
      format.splits(file).foreach(s => format.read(file, s).foreach(_ => ()))
      seen.get
    val slim = bytes[Slim]
    val wide = { final case class W(wide: String) derives Schema; bytes[W] }
    assert(slim * 5 < wide, s"Slim read $slim bytes, the wide column alone $wide: the wide column was decoded for Slim")
  }

  test("a split's rows that do not fit the type are refused with the file and the group named") {
    final case class Wrong(city: Long) derives Schema
    val e = intercept[IllegalStateException](localBulk.read(file, ParquetFile.rows[Wrong]).elements.toVector)
    assert(e.getMessage.contains(file) && e.getMessage.contains("row group 0"), e.getMessage)
  }
