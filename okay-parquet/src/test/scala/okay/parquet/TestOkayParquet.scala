package okay.parquet

import okay.arrow.{Column, Table}

/** our codec, on every platform (specs/parquet.md) */
class TestOkayParquet extends munit.FunSuite:

  import ParquetSamples.*

  test("every type we write round-trips, nulls and metadata included, in row groups") {
    val t = sample(5000)
    for compress <- Compress.values do
      val bytes = OkayParquet.write(t, groupRows = 1200, compress = compress)
      val f = OkayParquet.footer(ReadAt.of(bytes))
      assertEquals(f.groups, Vector(1200L, 1200L, 1200L, 1200L, 200L), s"$compress")
      assertEquals(f.metadata, Vector("written.by" -> "TestOkayParquet"))
      same(t, OkayParquet.read(ReadAt.of(bytes)))
  }

  test("a group, a column at a time: only the named columns") {
    val bytes = OkayParquet.write(sample(3000), groupRows = 1000)
    val in = ReadAt.of(bytes)
    val f = OkayParquet.footer(in)
    val g = OkayParquet.group(in, f, 2, Some(Set("name", "id")))
    assertEquals(g.cols.map(_._1), Vector("id", "name"))
    assertEquals(g.rows, 1000)
    g.cols.head._2 match
      case Column.Int64(v, _) => assertEquals(v(0), 2000L * 1000003L)
      case other => fail(s"id read back as ${Column.describe(other)}")
  }

  test("the reader reads one group's chunks, never the file") {
    val bytes = OkayParquet.write(sample(20000), groupRows = 2000)
    var largest = 0
    var total = 0L
    val counting = new ReadAt:
      def size: Long = bytes.length.toLong
      def read(offset: Long, len: Int): Array[Byte] =
        largest = math.max(largest, len); total += len
        ReadAt.of(bytes).read(offset, len)
    val f = OkayParquet.footer(counting)
    for g <- f.groups.indices do OkayParquet.group(counting, f, g): Unit
    assert(largest < bytes.length / 5, s"one read of $largest bytes from a file of ${bytes.length}")
    assert(total < bytes.length + bytes.length / 10, s"$total bytes read from a file of ${bytes.length}")
  }

  test("an empty table is a file with no rows") {
    val t = Table(Vector("x" -> Column.Int64(Array.emptyLongArray, Array.emptyBooleanArray)), Vector.empty)
    val back = OkayParquet.read(ReadAt.of(OkayParquet.write(t)))
    assertEquals(back.rows, 0)
    assertEquals(back.cols.map(_._1), Vector("x"))
  }

  test("what is not a Parquet file, or not one we read, is refused by name") {
    val e1 = intercept[Refused](OkayParquet.footer(ReadAt.of("not parquet at all".getBytes)))
    assert(e1.getMessage.contains("PAR1"), e1.getMessage)
    val good = OkayParquet.write(sample(100))
    val e2 = intercept[Refused](OkayParquet.footer(ReadAt.of(good.dropRight(20) ++ good.takeRight(8))))
    assert(e2.getMessage.contains("footer") || e2.getMessage.contains("Thrift"), e2.getMessage)
    val nested = Table(Vector("xs" -> Column.ListOf(Array(0, 1), Column.Int64(Array(1L), Array(true)), Array(true))), Vector.empty)
    val e3 = intercept[Refused](OkayParquet.write(nested))
    assert(e3.getMessage.contains("not written yet"), e3.getMessage)
  }

  test("with no import the codec is ours") {
    assertEquals(summon[ParquetCodec].name, "okay")
  }
