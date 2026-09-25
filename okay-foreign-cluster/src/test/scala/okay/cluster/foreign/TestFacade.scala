package okay.cluster.foreign

import okay.codec.Schema
import okay.%
import FacadeConformance.Rec

/** a module type with `Calls` and nothing else: the typeclass is open */
final case class EchoModule(tag: String)
object EchoModule:
  given Calls[EchoModule] = new:
    def name = "echo"
    def call[A: Schema, B: Schema](module: EchoModule, fn: String)(a: A): Either[Batcher.Failed, B] = fn match
      // an identity-shaped call at one type: the fake answers what it was
      // asked, and the JSON codec is how it says so at B without a cast
      case "echo" => okay.codec.Codecs.json(summon[Schema[B]]).decode(okay.codec.Json.parse(okay.codec.Json.write(a)))
        .left.map(m => Batcher.Failed("Decode", m))
      case "boom" => Left(Batcher.Failed("ValueError", "nope"))
      case other => Left(Batcher.Failed("AttributeError", s"no $other"))

/** a module type whose `Frames` COUNTS what it is handed: what the
 * stream road sends is then a number, not a belief */
final case class CountingModule(tag: String)
object CountingModule:
  val sizes = scala.collection.mutable.ArrayBuffer[Int]()
  given Frames[CountingModule] = new:
    def name = "counting"
    def frame(module: CountingModule, fn: String)(in: okay.arrow.Table): Either[Batcher.Failed, okay.arrow.Table] =
      sizes += in.rows
      Right(in)

/** a module type whose frames cross by a REAL road of the model without
 * a far side: "arrow" is an Arrow IPC stream written and read back (what
 * a worker speaking Arrow does to a table), "columnar-json" the columnar
 * JSON frame Python's shim is sent and answers (ArrowFrames, both ways).
 * `Speaks` says the road honestly; a test that wants a lie builds one. */
final case class RoadModule(road: String)
object RoadModule:
  given Calls[RoadModule] = new:
    def name = "road"
    def call[A: Schema, B: Schema](module: RoadModule, fn: String)(a: A): Either[Batcher.Failed, B] =
      summon[Calls[EchoModule]].call[A, B](EchoModule(module.road), fn)(a)
  given Frames[RoadModule] = new:
    def name = "road"
    def frame(module: RoadModule, fn: String)(in: okay.arrow.Table): Either[Batcher.Failed, okay.arrow.Table] =
      module.road match
        case "arrow" => Right(okay.arrow.OkayArrow.read(okay.arrow.OkayArrow.write(in)))
        case _ => okay.py.ArrowFrames.table(okay.py.ArrowFrames.frame(in)).left.map(m => Batcher.Failed("Frame", m))
  given Speaks[RoadModule] = new:
    def speaks(module: RoadModule) = Speaks.Report("road", "fake", module.road, stream = false, "none")

/** a module type that gives NO instance */
final case class Mute(tag: String)

/** specs/foreign-facade.md stage 1: one job text over every `Calls`
 * instance, a missing capability a compile error, `Speaks` for the JVM */
class TestFacade extends munit.FunSuite:

  val jvm = JvmModule("facade")
    .fn[Rec, Rec]("echo")(identity)
    .fn[Rec, Rec]("boom")(_ => throw IllegalStateException("nope"))
    .frame("fecho")(identity)
    .frame("fboom")(_ => throw IllegalStateException("nope"))

  test("Frames: the conformance body over the JVM's own module") {
    FacadeConformance.frames(jvm, "fecho", "fboom")
  }

  test("Streams: 10 000 rows through a counting Frames in frames of 1 000 — ten frames, none bigger, every row back") {
    CountingModule.sizes.clear()
    FacadeConformance.streams(CountingModule("t"), "any", 10000, 1000, () => CountingModule.sizes.toVector)
    assertEquals(CountingModule.sizes.length, 10)
  }

  test("Streams over the JVM's own module: the same body") {
    FacadeConformance.streams(jvm, "fecho", 5000, 512)
  }

  test("Frames on the JVM cross by REFERENCE: the same Table object comes back") {
    val t = okay.arrow.Rows.table(Vector(Rec(1, 1.0, "x")))
    val back = summon[Frames[JvmModule]].frame(jvm, "fecho")(t)
    assert(back.exists(_ eq t), "the table was copied")
    assertEquals(summon[Frames[JvmModule]].frame(jvm, "echo")(t).left.map(_.kind), Left("NoSuchFunction"))
  }

  test("Calls: the conformance body over the JVM's own module") {
    FacadeConformance.calls(jvm, "echo", "boom")
  }

  test("Calls: the same body over a test's own module type") {
    FacadeConformance.calls(EchoModule("t"), "echo", "boom")
  }

  test("a capability a module type does not give is a compile error, not a refusal") {
    assert(compileErrors("summon[Calls[Mute]]").nonEmpty)
    assert(compileErrors("summon[Speaks[Mute]]").nonEmpty)
    assert(compileErrors("summon[Frames[Mute]]").nonEmpty)
    assert(compileErrors("summon[Streams[Mute]]").nonEmpty)
    // programs as data have no JVM instance: nothing crosses (Decision 7)
    assert(compileErrors("summon[Programs[JvmModule]]").nonEmpty)
    // R holds objects but has no methods to call on them: Holds yes, Methods no
    assert(compileErrors("summon[Holds[okay.r.RModule]]").isEmpty)
    assert(compileErrors("summon[Methods[okay.r.RModule]]").nonEmpty)
    // a handle from Holds is what Methods takes: the two refined givens agree
    assert(compileErrors("val H = summon[Holds[okay.py.PyModule]]; val M = summon[Methods[okay.py.PyModule]]; (r: H.Ref) => (r: M.Ref)").isEmpty)
    assert(compileErrors("summon[Programs[okay.py.PyModule]]").isEmpty)
    // a Frames instance is a Streams instance: the derived road
    assert(compileErrors("summon[Streams[CountingModule]]").isEmpty)
    // EchoModule gives Calls and not Frames: rows through it do not compile
    assert(compileErrors("Road.rows[EchoModule, Rec, Rec](EchoModule(\"t\"), \"echo\")(Vector.empty)").nonEmpty)
    // and the one it gives compiles
    assert(compileErrors("summon[Calls[EchoModule]]").isEmpty)
  }

  test("Cb: a callback carries its name and its two schemas, and runs at its types") {
    val price = Cb[okay.Reader % Map[String, Double], String, Double]("price_of")(sku => okay.Reader.ask[Map[String, Double]].map(_(sku)))
    assertEquals(price.name, "price_of")
    // its schemas are the ones it was given, at its own types; running it
    // is the far side's business (FacadeConformance.programs, Live)
    assertEquals(price.arg, summon[Schema[String]])
    assertEquals(price.res, summon[Schema[Double]])
  }

  test("ONE JOB TEXT: a call, a frame and a stream, the module the only change — JVM, Arrow road, JSON road answer alike") {
    val there = FacadeConformance.job(jvm, "echo", "fecho")
    assertEquals(FacadeConformance.job(RoadModule("arrow"), "echo", "any"), there)
    assertEquals(FacadeConformance.job(RoadModule("columnar-json"), "echo", "any"), there)
    assertEquals(there._2, there._3, "the frame and the stream carry the same rows")
    // and the job does not compile against a module without every capability it names
    assert(compileErrors("""FacadeConformance.job(EchoModule("t"), "echo", "fecho")""").contains("Frames"))
  }

  test("Speaks agrees with what the worker DOES: the JVM, the Arrow road, the JSON road") {
    val r = FacadeConformance.agree(jvm, "fecho", "pairs")
    assertEquals(r.frames, "by-reference")
    assertEquals(FacadeConformance.agree(RoadModule("arrow"), "any", "pairs").frames, "arrow")
    assertEquals(FacadeConformance.agree(RoadModule("columnar-json"), "any", "pairs").frames, "columnar-json")
  }

  /** a report that says `change` of what it honestly would */
  def lying[M](honest: Speaks[M])(change: Speaks.Report => Speaks.Report): Speaks[M] = new:
    def speaks(module: M) = change(honest.speaks(module))

  test("Speaks disagreeing fails in BOTH directions: a claim nothing backs, an ability nothing claims") {
    def refused[M](module: M, lie: Speaks[M])(using f: Frames[M], p: FacadeConformance.Maybe[Programs[M]]): String =
      intercept[AssertionError](FacadeConformance.agree(module, "fecho", "pairs")(using lie, f, p)).getMessage
    val jvmS = summon[Speaks[JvmModule]]
    val roadS = summon[Speaks[RoadModule]]
    // frames claimed faster than they cross: the JSON road called Arrow
    assert(refused(RoadModule("columnar-json"), lying(roadS)(_.copy(frames = "arrow"))).contains("frames"))
    // frames crossing better than claimed: the Arrow road called JSON
    assert(refused(RoadModule("arrow"), lying(roadS)(_.copy(frames = "columnar-json"))).contains("frames"))
    // by reference, said to be Arrow
    assert(refused(jvm, lying(jvmS)(_.copy(frames = "arrow"))).contains("frames"))
    // programs claimed with no instance to run them
    assert(refused(jvm, lying(jvmS)(_.copy(programs = "multi-shot"))).contains("programs"))
    assert(refused(RoadModule("arrow"), lying(roadS)(_.copy(programs = "one-shot"))).contains("programs"))
    // a far-side stream claimed, which nothing observes yet
    assert(refused(jvm, lying(jvmS)(_.copy(stream = true))).contains("stream"))
  }

  test("Speaks: the JVM is in-jvm, by-reference") {
    val r = FacadeConformance.speaks(jvm, "jvm")
    assertEquals((r.link, r.frames, r.programs), ("in-jvm", "by-reference", "in-jvm"))
  }

  test("a JVM function that throws is a refusal by its class; a missing one is refused by name") {
    assertEquals(summon[Calls[JvmModule]].call[Rec, Rec](jvm, "boom")(Rec(1, 1.0, "x")),
      Left(Batcher.Failed("IllegalStateException", "nope")))
    summon[Calls[JvmModule]].call[Rec, Rec](jvm, "nope")(Rec(1, 1.0, "x")) match
      case Left(Batcher.Failed("NoSuchFunction", m)) => assert(m.contains("'nope'") && m.contains("boom, echo"), m)
      case other => fail(s"$other")
  }
