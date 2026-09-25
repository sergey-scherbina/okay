package okay.py

import okay.codec.ArrowIpc
import okay.codec.ArrowIpc.{Column, Table}

object PyArrow:
  /** a python with pyarrow: `OKAY_PYARROW_PYTHON`, else python3 when it has it */
  lazy val python: Option[String] =
    sys.env.get("OKAY_PYARROW_PYTHON").orElse(TestPy.python).filter { py =>
      scala.util.Try(ProcessBuilder(py, "-c", "import pyarrow").start().waitFor() == 0).getOrElse(false)
    }

  /** run a script with the stream file as argv[1]; its stdout */
  def run(script: String, file: java.nio.file.Path): String =
    val p = ProcessBuilder(python.get, "-c", script, file.toString).redirectErrorStream(true).start()
    val out = String(p.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
    assertExit(p.waitFor(), out)
    out.trim

  private def assertExit(code: Int, out: String): Unit =
    if code != 0 then throw IllegalStateException(s"python exited $code: $out")

/**
 * py-arrow stage 1, against the real thing: pyarrow reads (and fully
 * validates) the streams `ArrowIpc` writes, and `ArrowIpc` reads the
 * streams pyarrow writes, including shapes this side never makes —
 * several record batches, a validity buffer left out.
 */
class TestArrowPy extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = PyArrow.python.isEmpty

  private def tmp(bytes: Array[Byte]): java.nio.file.Path =
    val f = java.nio.file.Files.createTempFile("okay-arrow", ".arrows")
    java.nio.file.Files.write(f, bytes)

  private def fromPy(script: String): Table =
    val f = java.nio.file.Files.createTempFile("okay-arrow", ".arrows")
    PyArrow.run(script, f): Unit
    ArrowIpc.read(java.nio.file.Files.readAllBytes(f))

  test("pyarrow reads and validates what ArrowIpc writes: types, nulls, NaN, text, metadata") {
    val t = Table(Vector(
      "id" -> Column.Int64(Array(1L, -2L, Long.MaxValue, 0L), Array(true, true, true, false)),
      "temp" -> Column.Float64(Array(0.5, Double.NaN, -0.0, 1e300), Array(true, true, false, true)),
      "site" -> Column.Utf8(Array("kyiv", "", "чай ☕", "x"), Array(true, true, true, false)),
      "ok" -> Column.Bool(Array(true, false, true, true), Array(true, true, true, true)),
      "nothing" -> Column.Nulls(4)),
      Vector("okay" -> """{"id":7}"""))
    val out = PyArrow.run("""
import sys, json, math, pyarrow.ipc as ipc
t = ipc.open_stream(open(sys.argv[1], "rb").read()).read_all()
t.validate(full=True)
def cell(v):
    return "nan" if isinstance(v, float) and math.isnan(v) else v
print(json.dumps({"types": [str(f.type) for f in t.schema],
                  "meta": {k.decode(): v.decode() for k, v in (t.schema.metadata or {}).items()},
                  "rows": [[cell(v) for v in r.values()] for r in t.to_pylist()]}, ensure_ascii=False))
""", tmp(ArrowIpc.write(t)))
    assertEquals(out,
      """{"types": ["int64", "double", "string", "bool", "null"], "meta": {"okay": "{\"id\":7}"}, """ +
      """"rows": [[1, 0.5, "kyiv", true, null], [-2, "nan", "", false, null], """ +
      """[9223372036854775807, null, "чай ☕", true, null], [null, 1e+300, null, true, null]]}""")
  }

  test("ArrowIpc reads what pyarrow writes: every type, nulls, a column with no validity buffer") {
    val t = fromPy("""
import sys, pyarrow as pa, pyarrow.ipc as ipc
t = pa.table({"i": pa.array([1, None, 3], pa.int64()), "f": [1.5, float("nan"), None],
              "s": ["a", None, "ü"], "b": [True, None, False], "n": pa.nulls(3), "all": [7, 8, 9]},
             metadata={"okay": "{}"})
with ipc.new_stream(sys.argv[1], t.schema) as w: w.write_table(t)
""")
    assertEquals(t.metadata, Vector("okay" -> "{}"))
    assertEquals(t.cols.map(_._1), Vector("i", "f", "s", "b", "n", "all"))
    t.cols.map(_._2) match
      case Vector(Column.Int64(i, iv), Column.Float64(f, fv), Column.Utf8(s, sv), Column.Bool(b, bv), Column.Nulls(3), Column.Int64(all, allv)) =>
        assertEquals((i.toVector, iv.toVector), (Vector(1L, i(1), 3L), Vector(true, false, true)))
        assert(f(0) == 1.5 && f(1).isNaN && fv.toVector == Vector(true, true, false))
        assertEquals((s(0), s(2), sv.toVector), ("a", "ü", Vector(true, false, true)))
        assertEquals((b(0), b(2), bv.toVector), (true, false, Vector(true, false, true)))
        assertEquals((all.toVector, allv.toVector), (Vector(7L, 8L, 9L), Vector(true, true, true)))
      case other => fail(s"columns: $other")
  }

  test("several record batches are one table, in order") {
    val t = fromPy("""
import sys, pyarrow as pa, pyarrow.ipc as ipc
s = pa.schema([("x", pa.int64()), ("s", pa.string())])
with ipc.new_stream(sys.argv[1], s) as w:
    for k in range(3):
        w.write_batch(pa.record_batch([pa.array([k * 10 + j for j in range(4)]), pa.array(["r%d" % (k * 10 + j) for j in range(4)])], schema=s))
""")
    t.cols match
      case Vector(("x", Column.Int64(x, _)), ("s", Column.Utf8(s, _))) =>
        assertEquals(x.toVector, Vector(0L, 1, 2, 3, 10, 11, 12, 13, 20, 21, 22, 23))
        assertEquals(s.toVector.last, "r23")
      case other => fail(s"columns: $other")
  }

  test("a type outside the five is refused by name, with what to cast it to") {
    val e = intercept[IllegalStateException](fromPy("""
import sys, pyarrow as pa, pyarrow.ipc as ipc
t = pa.table({"small": pa.array([1, 2], pa.int32())})
with ipc.new_stream(sys.argv[1], t.schema) as w: w.write_table(t)
"""))
    assert(e.getMessage.contains("column 'small' is int32; cast it to int64"), e.getMessage)
  }

  test("an empty table crosses both ways") {
    val empty = Table(Vector("a" -> Column.Int64(Array.emptyLongArray, Array.emptyBooleanArray)), Vector.empty)
    assertEquals(PyArrow.run("""
import sys, pyarrow.ipc as ipc
t = ipc.open_stream(open(sys.argv[1], "rb").read()).read_all()
t.validate(full=True); print(t.num_rows, t.schema.names)
""", tmp(ArrowIpc.write(empty))), "0 ['a']")
    val back = fromPy("""
import sys, pyarrow as pa, pyarrow.ipc as ipc
t = pa.table({"a": pa.array([], pa.int64())})
with ipc.new_stream(sys.argv[1], t.schema) as w: w.write_table(t)
""")
    assertEquals(back.rows, 0)
  }
