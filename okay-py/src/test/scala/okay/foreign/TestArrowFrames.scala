package okay.foreign

import PyValue.*

object ArrowConf:
  val mod = Foreign.module("arrowconf", """
    import okay

    def identity(frame):
        return frame

    @okay.arrow
    def what(t):
        return {"kind": [type(t).__name__], "rows": [t.num_rows]}

    @okay.arrow
    def narrow(t):
        import pyarrow as pa
        return pa.table({"x": pa.array([1, 2], pa.int32()), "y": pa.array([0.5, None], pa.float32())})

    @okay.arrow
    def same(t):
        return t

    def lists(frame):
        return {"xs": [[1], [2, 3]]}

    def kinds(frame):
        return {k: [type(v).__name__ for v in col] for k, col in frame.items()}
  """)

/** py-arrow stages 2–3: a real worker with pyarrow, frames as Arrow */
class TestArrowFrames extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = PyArrow.python.isEmpty

  lazy val engine: ForeignWorker = ForeignWorker.start(PyArrow.python.get, modules = Seq(ArrowConf.mod))
  override def afterAll(): Unit = if !munitIgnore then engine.close()
  private def frame(fn: String, f: PyFrame, w: ForeignWorker = engine) =
    w.handler.handle(ForeignEval.Frame(s"arrowconf:$fn", f, Vector.empty))

  private val mixed = PyFrame(Vector(
    "id" -> Vector(I64(1), I64(-2), PyNone),
    "temp" -> Vector(F64(0.5), PyNone, F64(1e300)),
    "site" -> Vector(Str("kyiv"), Str("чай ☕"), PyNone),
    "ok" -> Vector(Bool(true), PyNone, Bool(false)),
    "nothing" -> Vector(PyNone, PyNone, PyNone)))

  test("with no import, a worker with pyarrow takes frames as Arrow") {
    assertEquals(engine.wire, "json/none+arrow")
  }

  test("a frame round-trips as Arrow: every column kind, None in each, text beyond ASCII") {
    val (out, in) = engine.arrowFrames
    assertEquals(frame("identity", mixed), Right(mixed))
    assertEquals(engine.arrowFrames, (out + 1, in + 1))
  }

  test("the function still gets a dict of lists, with Python's own types") {
    assertEquals(frame("kinds", mixed).map(_.cols.toMap.get("id")),
      Right(Some(Vector(Str("int"), Str("int"), Str("NoneType")))))
  }

  test("@okay.arrow hands the function the pyarrow.Table itself") {
    assertEquals(frame("what", mixed), Right(PyFrame(Vector("kind" -> Vector(Str("Table")), "rows" -> Vector(I64(3))))))
  }

  test("an answer in other Arrow types is normalised: int32 to int64, float32 to float64") {
    assertEquals(frame("narrow", mixed), Right(PyFrame(Vector("x" -> Vector(I64(1), I64(2)), "y" -> Vector(F64(0.5), PyNone)))))
  }

  test("an answer Arrow cannot carry here comes back as the JSON frame it always was") {
    val (out, in) = engine.arrowFrames
    assertEquals(frame("lists", mixed), Right(PyFrame(Vector("xs" -> Vector(Arr(Vector(I64(1))), Arr(Vector(I64(2), I64(3))))))))
    assertEquals(engine.arrowFrames, (out + 1, in))
  }

  test("a request Arrow cannot carry takes the JSON road by default, and is refused by name under the strict given") {
    val odd = PyFrame(Vector("n" -> Vector(I64(1), F64(2.5))))
    val before = engine.arrowFrames
    assertEquals(frame("identity", odd), Right(odd))
    assertEquals(engine.arrowFrames, before)
    import FrameFormat.Arrow.given
    val strict = ForeignWorker.start(PyArrow.python.get, modules = Seq(ArrowConf.mod))
    try
      val got = frame("identity", odd, strict)
      assert(got.left.exists(c => c.kind == "NotArrow" && c.message.contains("column 'n' mixes kinds (floats among ints)")), got.toString)
    finally strict.close()
  }

  test("FrameFormat.Json turns it off: the plain wire, and frames still cross") {
    import FrameFormat.Json.given
    val w = ForeignWorker.start(PyArrow.python.get, modules = Seq(ArrowConf.mod))
    try
      assertEquals(w.wire, "json/none")
      assertEquals(frame("identity", mixed, w), Right(mixed))
    finally w.close()
  }

  test("CBOR and DEFLATE compose with Arrow: the header rides the metadata, the whole message is compressed") {
    import WireFormat.Cbor.given
    import WireCompression.Deflate.given
    val w = ForeignWorker.start(PyArrow.python.get, modules = Seq(ArrowConf.mod))
    try
      assertEquals(w.wire, "cbor/deflate+arrow")
      assertEquals(frame("identity", mixed, w), Right(mixed))
      assertEquals(w.arrowFrames, (1L, 1L))
    finally w.close()
  }

  test("a strict Arrow host refuses a worker without pyarrow by name") {
    val plain = TestPy.python.filter(py => !PyArrow.python.contains(py) ||
      !scala.util.Try(ProcessBuilder(py, "-c", "import pyarrow").start().waitFor() == 0).getOrElse(false))
    assume(plain.nonEmpty, "every python here has pyarrow")
    import FrameFormat.Arrow.given
    val e = intercept[IllegalStateException](ForeignWorker.start(plain.get))
    assert(e.getMessage.contains("speaks the frames json; this host's given FrameFormat is arrow"), e.getMessage)
  }
