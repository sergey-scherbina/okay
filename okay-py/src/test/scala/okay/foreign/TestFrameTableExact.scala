package okay.foreign

import okay.arrow.{Column, Table, TimeUnit}

object ExactEcho:
  val mod = Foreign.module("exactecho", """
import okay

@okay.arrow
def echo(tbl):
    return tbl

@okay.arrow
def via_pandas(tbl):
    # a round trip through pandas on ARROW-BACKED dtypes, the input's schema
    # out. (numpy's float64 cannot hold a NaN beside a null — both are NaN
    # there — so a plain to_pandas() loses the difference; that is pandas)
    import pyarrow as pa, pandas as pd
    df = tbl.to_pandas(types_mapper=pd.ArrowDtype)
    return pa.Table.from_pandas(df, schema=tbl.schema, preserve_index=False)
""")

/** okay-arrow stage 8: `frameTable(exact = true)` answers the far side's table as it made it (Live) */
class TestFrameTableExact extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = PyArrow.python.isEmpty
  // a python in a container, pandas imported cold: well past munit's 30 s
  override val munitTimeout = scala.concurrent.duration.Duration(3, "min")

  private lazy val w = ForeignWorker.start(PyArrow.python.get, modules = Seq(ExactEcho.mod))
  override def afterAll(): Unit = if PyArrow.python.nonEmpty then w.close()

  private val sent = Table(Vector(
    "level" -> Column.Dictionary(Array(0, 2, 0), Column.Utf8(Array("employed", "unemployed", "self-employed"), Array.fill(3)(true)),
      ordered = false, valid = Array(true, true, false)),
    "age" -> Column.Ints(32, true, Array(40L, 0L, 61L), Array(true, false, true)),
    "ratio" -> Column.Float64(Array(0.25, Double.NaN, 0.0), Array(true, true, false)),
    "born" -> Column.Date32(Array(-3650, 0, 20000), Array(true, true, false)),
    "at" -> Column.Timestamp(TimeUnit.Micro, Some("UTC"), Array(1790000000123456L, 0L, -1L), Array(true, false, true))),
    Vector.empty)

  /** a column as comparable cells: None for a null, the dictionary's values and order first */
  private def cells(c: Column): Vector[Any] =
    def each[A](v: Array[A], ok: Array[Boolean])(f: A => Any) = v.indices.map(i => Option.when(ok(i))(f(v(i)))).toVector
    c match
      case Column.Dictionary(idx, d, ord, ok) => Vector(cells(d), ord) ++ each(idx, ok)(identity)
      case Column.Utf8(v, ok) => each(v, ok)(identity)
      case Column.Ints(b, sg, v, ok) => Vector(b, sg) ++ each(v, ok)(identity)
      case Column.Float64(v, ok) => each(v, ok)(java.lang.Double.doubleToRawLongBits(_))
      case Column.Date32(v, ok) => each(v, ok)(identity)
      case Column.Timestamp(u, z, v, ok) => Vector(u, z) ++ each(v, ok)(identity)
      case other => Vector(Column.describe(other))

  private def same(got: Table): Unit =
    assertEquals(got.cols.map(_._1), sent.cols.map(_._1))
    got.cols.zip(sent.cols).foreach { case ((n, g), (_, s)) =>
      assertEquals(Column.describe(g), Column.describe(s), s"column $n's type")
      assertEquals(cells(g), cells(s), s"column $n's cells")
    }

  for fn <- Vector("echo", "via_pandas") do
    test(s"$fn: a dictionary, int32, NaN beside a null, date32 and a timestamp come back exactly") {
      same(w.frameTable(s"exactecho:$fn", sent, Vector.empty, exact = true).fold(c => fail(c.toString), identity))
    }

  test("without exact, the answer is narrowed to the frame's kinds, as before") {
    val got = w.frameTable("exactecho:echo", sent.copy(cols = sent.cols.take(3)), Vector.empty).fold(c => fail(c.toString), identity)
    assertEquals(got.cols.map((n, c) => n -> Column.describe(c)), Vector("level" -> "Utf8", "age" -> "Int64", "ratio" -> "Float64"))
  }
