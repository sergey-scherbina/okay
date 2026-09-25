package okay.cluster.foreign

import okay.codec.Schema
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
    // a Frames instance is a Streams instance: the derived road
    assert(compileErrors("summon[Streams[CountingModule]]").isEmpty)
    // EchoModule gives Calls and not Frames: rows through it do not compile
    assert(compileErrors("Road.rows[EchoModule, Rec, Rec](EchoModule(\"t\"), \"echo\")(Vector.empty)").nonEmpty)
    // and the one it gives compiles
    assert(compileErrors("summon[Calls[EchoModule]]").isEmpty)
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
