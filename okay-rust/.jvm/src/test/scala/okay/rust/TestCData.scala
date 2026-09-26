package okay.rust

import okay.arrow.{Column, Table}
import okay.py.{ForeignEval, ForeignWorker, PyFrame, PyValue}

/** a table's cells as plain values, so two tables compare */
object Cells:
  def of(t: Table): Vector[(String, Vector[Option[Any]])] = t.cols.map { (name, c) =>
    name -> (c match
      case Column.Int64(v, ok) => v.indices.toVector.map(i => Option.when(ok(i))(v(i)))
      case Column.Float64(v, ok) => v.indices.toVector.map(i => Option.when(ok(i))(v(i)))
      case Column.Utf8(v, ok) => v.indices.toVector.map(i => Option.when(ok(i))(v(i)))
      case Column.Bool(v, ok) => v.indices.toVector.map(i => Option.when(ok(i))(v(i)))
      case Column.Nulls(n) => Vector.fill(n)(None)
      case other => fail(s"not a column this suite makes: $other"))
  }
  private def fail(why: String): Nothing = throw AssertionError(why)

  /** every kind a frame makes, with nulls, an empty string and a character past ASCII */
  val table: Table = Table(Vector(
    "id" -> Column.Int64(Array(1L, -2L, 0L, Long.MaxValue), Array(true, true, false, true)),
    "temp" -> Column.Float64(Array(0.5, Double.NaN, 0.0, 1e300), Array(true, true, false, true)),
    "site" -> Column.Utf8(Array("kyiv", "чай ☕", null, ""), Array(true, true, false, true)),
    "ok" -> Column.Bool(Array(true, false, false, true), Array(true, true, false, true)),
    "nothing" -> Column.Nulls(4)), Vector.empty)

/**
 * The C Data codecs, ours and Apache Arrow's (foreign-arrow-ffm): each reads
 * what the other wrote, cell for cell — the own-or-standard rule's proof —
 * with no library in between. Default gate.
 */
class TestCData extends munit.FunSuite:
  private def through(out: CDataCodec, in: CDataCodec, t: Table): Table =
    out.exporting(t)((schema, array) => in.importing(schema, array))

  private val pairs = for a <- Seq(OkayCData, ApacheCData); b <- Seq(OkayCData, ApacheCData) yield (a, b)

  for (a, b) <- pairs do
    test(s"exported by ${a.name}, read by ${b.name}: every cell, nulls and all") {
      assertEquals(Cells.of(through(a, b, Cells.table)).toString, Cells.of(Cells.table).toString)
    }
    test(s"exported by ${a.name}, read by ${b.name}: a table of no rows") {
      val empty = Table(Vector("id" -> Column.Int64(Array.empty, Array.empty)), Vector.empty)
      assertEquals(Cells.of(through(a, b, empty)), Cells.of(empty))
    }

  test("a column C Data does not carry here is refused by name") {
    val listy = Table(Vector("xs" -> Column.Date32(Array(1), Array(true))), Vector.empty)
    val e = intercept[IllegalArgumentException](OkayCData.exporting(listy)((_, _) => ()))
    assert(e.getMessage.contains("int64, float64, utf8, boolean and null columns"), e.getMessage)
  }

/**
 * A table into in-process Rust as C Data (foreign-arrow-ffm): the conformance
 * crate's `echo` answers what it was given, `mixed` a column C Data cannot
 * carry. Live: it builds the crate.
 */
class TestRustCData extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !RustInProcess.available

  private val frame = PyFrame(Vector(
    "id" -> Vector(PyValue.I64(1), PyValue.I64(-2), PyValue.PyNone),
    "temp" -> Vector(PyValue.F64(0.5), PyValue.PyNone, PyValue.F64(1e300)),
    "site" -> Vector(PyValue.Str("kyiv"), PyValue.Str("чай ☕"), PyValue.PyNone),
    "ok" -> Vector(PyValue.Bool(true), PyValue.PyNone, PyValue.Bool(false)),
    "nothing" -> Vector(PyValue.PyNone, PyValue.PyNone, PyValue.PyNone)))

  private def roundTrip(w: ForeignWorker): Unit =
    val before = w.arrowFrames
    assertEquals(w.handler.handle(ForeignEval.Frame("echo", frame, Vector.empty)).map(_.cols), Right(frame.cols))
    assertEquals(w.arrowFrames, (before._1 + 1, before._2 + 1), "the table went out and came back as C Data")

  test("a table goes to Rust and back as C Data, every kind and null intact (ours, the default)") {
    val w = ForeignWorker.inProcess(RustInProcess.dylib)
    try roundTrip(w) finally w.close()
  }

  test("the same through Apache Arrow's arrow-c-data, by an import") {
    import ApacheCData.given
    val w = ForeignWorker.inProcess(RustInProcess.dylib)
    try roundTrip(w) finally w.close()
  }

  test("an answer C Data cannot carry comes back on the wire instead, the same values") {
    val w = ForeignWorker.inProcess(RustInProcess.dylib)
    try
      val before = w.arrowFrames
      assertEquals(w.handler.handle(ForeignEval.Frame("mixed", frame, Vector.empty)).map(_.cols),
        Right(Vector("m" -> Vector(PyValue.I64(1), PyValue.Str("two")))))
      assertEquals(w.arrowFrames, (before._1 + 1, before._2), "out as C Data, back as a message")
    finally w.close()
  }
