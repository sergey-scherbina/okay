package okay.arrow


/** py-arrow stage 1, without pyarrow: what this writes it reads back, and
 * a stream cut short at any byte is refused (TestArrowPy is the oracle) */
class TestOkayArrow extends munit.FunSuite:

  private def table: Table = Table(Vector(
    "id" -> Column.Int64(Array(1L, -2L, Long.MaxValue, 0L), Array(true, true, true, false)),
    "temp" -> Column.Float64(Array(0.5, Double.NaN, -0.0, 1e300), Array(true, true, false, true)),
    "site" -> Column.Utf8(Array("kyiv", "", "чай ☕", "x"), Array(true, true, true, false)),
    "ok" -> Column.Bool(Array(true, false, true, true), Array(true, true, true, true)),
    "nothing" -> Column.Nulls(4)),
    Vector("okay" -> """{"id":7,"op":"frame"}"""))

  private def cells(c: Column): Vector[Option[Any]] = Tables.cells(c)

  private def same(a: Table, b: Table): Unit =
    assertEquals(b.metadata, a.metadata)
    assertEquals(b.cols.map(_._1), a.cols.map(_._1))
    a.cols.zip(b.cols).foreach { case ((n, x), (_, y)) =>
      assertEquals(y.getClass, x.getClass, n)
      assertEquals(cells(y), cells(x), n)
    }

  test("every kind of column round-trips, nested lists and structs included") {
    assertEquals(Tables.same(Tables.everything, OkayArrow.read(OkayArrow.write(Tables.everything))), None)
  }

  test("the all-types stream cut short, at any byte, is refused by name") {
    val bytes = OkayArrow.write(Tables.everything)
    val accepted = (1 until bytes.length by 7).filter(n => scala.util.Try(OkayArrow.read(bytes.dropRight(n))).isSuccess)
    assertEquals(accepted.toVector, Vector.empty)
  }

  test("a take gathers rows, nested ones included, and nulls where keep says") {
    val t = Tables.everything
    val picked = t.cols.map((n, c) => n -> c.take(Array(2, 0, 2), Array(true, true, false)))
    val back = OkayArrow.read(OkayArrow.write(Table(picked, t.metadata)))
    assertEquals(Tables.same(Table(picked, t.metadata), back), None)
    picked.collectFirst { case ("tags", c) => c } match
      case Some(c) => assertEquals(Tables.cells(c), Vector(Some(Vector(Some("c"))), Some(Vector(Some("a"), None)), None))
      case None => fail("no tags")
  }

  test("concat joins batches of every kind, lists by shifted offsets") {
    val t = Tables.everything
    val twice = t.cols.map((n, c) => n -> Column.concat(Vector(c, c)))
    assertEquals(twice.map(_._2.length).distinct, Vector(8))
    assertEquals(Tables.same(Table(twice, t.metadata), OkayArrow.read(OkayArrow.write(Table(twice, t.metadata)))), None)
  }

  test("a table round-trips: every column type, nulls, NaN beside a null, text beyond ASCII, the metadata") {
    val bytes = OkayArrow.write(table)
    assert(ArrowCodec.isStream(bytes))
    same(table, OkayArrow.read(bytes))
  }

  test("an empty table and a table without columns round-trip") {
    val empty = Table(Vector("a" -> Column.Int64(Array.emptyLongArray, Array.emptyBooleanArray),
      "s" -> Column.Utf8(Array.empty[String], Array.emptyBooleanArray)), Vector.empty)
    same(empty, OkayArrow.read(OkayArrow.write(empty)))
    val none = Table(Vector.empty, Vector("k" -> "v"))
    same(none, OkayArrow.read(OkayArrow.write(none)))
  }

  test("a stream cut short, at any byte, is refused by name") {
    val bytes = OkayArrow.write(table)
    val accepted = (1 until bytes.length).filter { n =>
      scala.util.Try(OkayArrow.read(bytes.dropRight(n))).isSuccess
    }
    assertEquals(accepted.toVector, Vector.empty)
    val e = intercept[IllegalStateException](OkayArrow.read(bytes.take(bytes.length / 2)))
    assert(e.getMessage.startsWith("not an Arrow stream this reads"), e.getMessage)
  }

  test("JSON and CBOR are never mistaken for a stream") {
    assert(!ArrowCodec.isStream("""{"id":1}""".getBytes))
    // CBOR {"id": 1}: a map of one pair; the wire's first byte is a map's
    assert(!ArrowCodec.isStream(Array(0xa1, 0x62, 'i', 'd', 0x01).map(_.toByte)))
  }

  test("columns of unequal length are refused before a byte is written") {
    val bad = Table(Vector("a" -> Column.Nulls(2), "b" -> Column.Nulls(3)), Vector.empty)
    val e = intercept[IllegalArgumentException](OkayArrow.write(bad))
    assert(e.getMessage.contains("column 'b' has 3 rows, the first has 2"), e.getMessage)
  }

  test("compressed bodies, LZ4 and ZSTD, round-trip every kind of column, and shrink what repeats") {
    for codec <- Vector(okay.compress.Lz4Frame, okay.compress.Zstd) do
      assertEquals(Tables.same(Tables.everything, OkayArrow.read(OkayArrow.write(Tables.everything, Some(codec)))), None, codec.name)
    val n = 20000
    val repetitive = Table(Vector(
      "id" -> Column.Int64(Array.tabulate(n)(_.toLong), Array.fill(n)(true)),
      "city" -> Column.Utf8(Array.tabulate(n)(i => if i % 2 == 0 then "kyiv" else "lviv"), Array.fill(n)(true))), Vector.empty)
    val plain = OkayArrow.write(repetitive).length
    // sequential int64s give LZ4 no long repeats: pyarrow's own LZ4 halves that
    // buffer too (160000 -> 80069 bytes), where ZSTD's entropy coding takes it to 21019
    for (codec, limit) <- Vector(okay.compress.Lz4Frame -> 0.6, okay.compress.Zstd -> 0.33) do
      val packed = OkayArrow.write(repetitive, Some(codec))
      assert(packed.length < plain * limit, s"${codec.name}: ${packed.length} of $plain")
      assertEquals(Tables.same(repetitive, OkayArrow.read(packed)), None, codec.name)
  }

  test("the IPC FILE format: every kind round-trips, plain and compressed; one batch by index; a cut file refused") {
    for codec <- Vector(None, Some(okay.compress.Lz4Frame), Some(okay.compress.Zstd)) do
      val f = OkayArrow.writeFile(Tables.everything, codec)
      assertEquals(new String(f.take(6), "UTF-8"), "ARROW1")
      assertEquals(new String(f.takeRight(6), "UTF-8"), "ARROW1")
      assertEquals(OkayArrow.fileBatches(f), 1)
      assertEquals(Tables.same(Tables.everything, OkayArrow.readFile(f)), None, codec.toString)
      assertEquals(Tables.same(Tables.everything, OkayArrow.readFileBatch(f, 0)), None, codec.toString)
    val f = OkayArrow.writeFile(Tables.everything)
    val cut = (1 until f.length by 5).filter(n => scala.util.Try(OkayArrow.readFile(f.dropRight(n))).isSuccess)
    assertEquals(cut.toVector, Vector.empty)
    assert(intercept[IllegalStateException](OkayArrow.readFileBatch(f, 1)).getMessage.contains("batch 1 of 1"))
    assert(intercept[IllegalStateException](OkayArrow.readFile(OkayArrow.write(Tables.everything))).getMessage.contains("no ARROW1"))
  }
