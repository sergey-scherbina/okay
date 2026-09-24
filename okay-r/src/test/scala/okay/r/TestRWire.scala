package okay.r

import okay.{Choose, Reader, effect, runChoice, given}
import okay.codec.{WireCompression, WireFormat}
import RValue.*

object RWire:
  val conf = R.module("conf", """
    pairs <- function() {
      okay_then(okay_perform("choose", c(1, 2)), function(x)
        okay_then(okay_perform("choose", c(10, 20)), function(y)
          okay_done(x + y)))
    }
    # DIRECT STYLE: ordinary R calling okay's effects
    quote <- function(sku, qty) okay_call("discount", okay_call("price_of", sku) * qty)
    slow <- function() { Sys.sleep(30); 1 }
  """)

/**
 * wire-givens-r: ONE test body over every wire R speaks — whatever the
 * givens chose (JSON or CBOR, zlib or none), the answers are the same:
 * scalars and R's two absences, text beyond ASCII, frames with NA, a long
 * column (CBOR's one-writeBin road), callbacks (`okay_call`) under the
 * caller's Reader, programs continued twice, and a timeout's respawn, which
 * must negotiate the same wire again.
 */
abstract class RWireConformance extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  /** the wire this suite's givens must settle on */
  def expected: String
  def open(timeoutMillis: Option[Long] = None): RSubprocess

  private lazy val r = open()
  private given okay.Handler[REval] = r.handler
  override def afterAll(): Unit = if TestR.rscript.nonEmpty then r.close()

  private def call(fn: String, args: RValue*) = r.handler.handle(REval.Call(fn, args.toVector))

  test("the handshake settled on the wire the givens asked for") {
    assertEquals(r.wire, expected)
  }

  test("values and R's two absences, the same on every wire") {
    assertEquals(call("mean", Vec(Vector(F64(1), F64(2), NA(RType.Double)))), Right(Vec(Vector(NA(RType.Double)))))
    assertEquals(call("length", RNull), Right(Vec(Vector(I32(0)))))
    assertEquals(call("paste0", Str("чай "), Str("☕")), Right(Vec(Vector(Str("чай ☕")))))
    assertEquals(call("sqrt", Vec(Vector(F64(2)))), Right(Vec(Vector(F64(math.sqrt(2))))))
    val boom = call("stop", Str("deliberate"))
    assert(boom.left.exists(_.message.contains("deliberate")), boom.toString)
  }

  test("a frame with NA in place, and a long column, come back as they went") {
    val in = RFrame(Vector(
      "x" -> Vector(F64(1), NA(RType.Double), F64(3.5)),
      "s" -> Vector(Str("a"), NA(RType.Character), Str("ü"))))
    assertEquals(r.handler.handle(REval.Frame("identity", in, Vector.empty)), Right(in))
    val long = RFrame(Vector("v" -> Vector.tabulate(20000)(i => F64(i * 0.25))))
    assertEquals(r.handler.handle(REval.Frame("identity", long, Vector.empty)), Right(long))
  }

  test("DIRECT STYLE: R calls okay_call twice, answered under the caller's Reader") {
    val priceOf = R.callback[String, Double]("price_of")(sku => Reader.ask[Map[String, Double]].map(_(sku)))
    val discount = R.callback[Double, Double]("discount")(a => Reader.ask[Map[String, Double]].map(m => a * m("rate")))
    val quote = R.fn[Double]("conf::quote").calling(R.callbacks(priceOf, discount))("tea", 3.0)
    assertEquals(Reader.run(Map("tea" -> 4.0, "rate" -> 0.5))(quote).runWith, Right(6.0))
  }

  test("MULTI-SHOT: an R closure continued on every branch") {
    val choose = R.callback[Vector[Double], Double]("choose")(xs => effect[Choose, Double](Choose(xs)))
    val pairs = R.program[Double]("conf::pairs").calling(R.callbacks(choose))()
    assertEquals(runChoice(pairs.program).runWith.toList, List(Right(11.0), Right(21.0), Right(12.0), Right(22.0)))
    pairs.forget.runWith
  }

  test("a timeout's respawn negotiates the same wire, and the next call is answered on it") {
    val s = open(Some(3000L))
    try
      val late = s.handler.handle(REval.Call("conf::slow", Vector.empty))
      assert(late.left.exists(_.kind == "timeout"), late.toString)
      assertEquals(s.wire, expected)
      assertEquals(s.handler.handle(REval.Call("sqrt", Vector(Vec(Vector(F64(81)))))), Right(Vec(Vector(F64(9)))))
    finally s.close()
  }

/** no import: JSON, with zlib because R announces it */
class TestRWireDefault extends RWireConformance:
  def expected = "json/zlib"
  def open(timeoutMillis: Option[Long]) =
    RSubprocess.start(rscript = TestR.rscript.get, timeoutMillis = timeoutMillis, modules = Seq(RWire.conf))

/** compression turned off: the JSON lines R always spoke */
class TestRWireOff extends RWireConformance:
  import WireCompression.Off.given
  def expected = "json/none"
  def open(timeoutMillis: Option[Long]) =
    RSubprocess.start(rscript = TestR.rscript.get, timeoutMillis = timeoutMillis, modules = Seq(RWire.conf))

/** CBOR, with the default compression */
class TestRWireCbor extends RWireConformance:
  import WireFormat.Cbor.given
  def expected = "cbor/zlib"
  def open(timeoutMillis: Option[Long]) =
    RSubprocess.start(rscript = TestR.rscript.get, timeoutMillis = timeoutMillis, modules = Seq(RWire.conf))

/** CBOR, uncompressed */
class TestRWireCborPlain extends RWireConformance:
  import WireFormat.Cbor.given
  import WireCompression.Off.given
  def expected = "cbor/none"
  def open(timeoutMillis: Option[Long]) =
    RSubprocess.start(rscript = TestR.rscript.get, timeoutMillis = timeoutMillis, modules = Seq(RWire.conf))

  test("an explicit raw DEFLATE, which R does not speak, is refused by name") {
    import WireCompression.Deflate.given
    val e = intercept[IllegalStateException](RSubprocess.start(rscript = TestR.rscript.get))
    assert(e.getMessage.contains("the R shim speaks the compressions none, zlib; this host's given WireCompression is deflate"),
      e.getMessage)
  }
