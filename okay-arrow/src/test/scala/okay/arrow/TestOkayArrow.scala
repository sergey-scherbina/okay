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

  /** a column as comparable cells: None for a null */
  private def cells(c: Column): Vector[Option[Any]] = c match
    case Column.Int64(v, ok) => v.indices.map(i => Option.when(ok(i))(v(i))).toVector
    case Column.Float64(v, ok) => v.indices.map(i => Option.when(ok(i))(java.lang.Double.doubleToRawLongBits(v(i)))).toVector
    case Column.Utf8(v, ok) => v.indices.map(i => Option.when(ok(i))(v(i))).toVector
    case Column.Bool(v, ok) => v.indices.map(i => Option.when(ok(i))(v(i))).toVector
    case Column.Nulls(n) => Vector.fill(n)(None)

  private def same(a: Table, b: Table): Unit =
    assertEquals(b.metadata, a.metadata)
    assertEquals(b.cols.map(_._1), a.cols.map(_._1))
    a.cols.zip(b.cols).foreach { case ((n, x), (_, y)) =>
      assertEquals(y.getClass, x.getClass, n)
      assertEquals(cells(y), cells(x), n)
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
