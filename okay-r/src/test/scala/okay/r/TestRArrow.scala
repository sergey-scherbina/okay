package okay.r

import RValue.*

object RArrow:
  /** an Rscript with the `arrow` package: `OKAY_ARROW_RSCRIPT`, else
   * `TestR.rscript` when it happens to have the package (okay-py's
   * `PyArrow.python` twin) — no dedicated container image here: building
   * one that compiles the `arrow` package is its own cost, left to
   * whoever runs this suite for real */
  lazy val rscript: Option[String] =
    sys.env.get("OKAY_ARROW_RSCRIPT").orElse(TestR.rscript).filter { rs =>
      scala.util.Try(ProcessBuilder(rs, "-e",
        "quit(status = if (requireNamespace('arrow', quietly = TRUE)) 0L else 1L)")
        .start().waitFor() == 0).getOrElse(false)
    }

object RArrowConf:
  val mod = R.module("rarrowconf", """
    identity <- function(frame) frame
  """)

/** r-arrow: a real R with the `arrow` package, frames as Arrow IPC
 * streams — okay-py's `TestArrowFrames` twin */
class TestRArrow extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = RArrow.rscript.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  lazy val engine: RSubprocess = RSubprocess.start(RArrow.rscript.get, modules = Seq(RArrowConf.mod))
  override def afterAll(): Unit = if !munitIgnore then engine.close()
  private def frame(fn: String, f: RFrame, r: RSubprocess = engine) =
    r.handler.handle(REval.Frame(s"rarrowconf:$fn", f, Vector.empty))

  private val mixed = RFrame(Vector(
    "id" -> Vector(I32(1), I32(-2), NA(RType.Integer)),
    "temp" -> Vector(F64(0.5), NA(RType.Double), F64(1e300)),
    "site" -> Vector(Str("kyiv"), Str("чай ☕"), NA(RType.Character)),
    "ok" -> Vector(Bool(true), NA(RType.Logical), Bool(false))))

  test("with no import, a worker with the arrow package takes frames as Arrow") {
    assertEquals(engine.wire, "json/none+arrow")
  }

  test("a frame round-trips as Arrow: every column kind, NA in each, text beyond ASCII") {
    val (out, in) = engine.arrowFrames
    assertEquals(frame("identity", mixed), Right(mixed))
    assertEquals(engine.arrowFrames, (out + 1, in + 1))
  }

  test("a request the model cannot carry takes the JSON road by default, refused by name under the strict given") {
    val odd = RFrame(Vector("x" -> Vector(Bytes(Array[Byte](1)))))
    val before = engine.arrowFrames
    assertEquals(frame("identity", odd), Right(odd))
    assertEquals(engine.arrowFrames, before)
    import okay.codec.FrameFormat.Arrow.given
    val strict = RSubprocess.start(RArrow.rscript.get, modules = Seq(RArrowConf.mod))
    try
      val got = frame("identity", odd, strict)
      assert(got.left.exists(c => c.kind == "NotArrow" && c.message.contains("column 'x' holds raw")), got.toString)
    finally strict.close()
  }

  test("FrameFormat.Json turns it off: the plain wire, and frames still cross") {
    import okay.codec.FrameFormat.Json.given
    val r = RSubprocess.start(RArrow.rscript.get, modules = Seq(RArrowConf.mod))
    try
      assertEquals(r.wire, "json/none")
      assertEquals(frame("identity", mixed, r), Right(mixed))
    finally r.close()
  }

  test("CBOR and zlib compose with Arrow: the header rides the metadata, the whole message is compressed") {
    import okay.codec.WireFormat.Cbor.given
    import okay.codec.WireCompression.Zlib.given
    val r = RSubprocess.start(RArrow.rscript.get, modules = Seq(RArrowConf.mod))
    try
      assertEquals(r.wire, "cbor/zlib+arrow")
      assertEquals(frame("identity", mixed, r), Right(mixed))
      assertEquals(r.arrowFrames, (1L, 1L))
    finally r.close()
  }

  test("a strict Arrow host refuses a worker without the arrow package by name") {
    val plain = TestR.rscript.filter(rs => !RArrow.rscript.contains(rs))
    assume(plain.nonEmpty, "every R here has the arrow package")
    import okay.codec.FrameFormat.Arrow.given
    val e = intercept[IllegalStateException](RSubprocess.start(plain.get))
    assert(e.getMessage.contains("speaks the frames json; this host's given FrameFormat is arrow"), e.getMessage)
  }
