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

/** a module type that gives NO instance */
final case class Mute(tag: String)

/** specs/foreign-facade.md stage 1: one job text over every `Calls`
 * instance, a missing capability a compile error, `Speaks` for the JVM */
class TestFacade extends munit.FunSuite:

  val jvm = JvmModule("facade")
    .fn[Rec, Rec]("echo")(identity)
    .fn[Rec, Rec]("boom")(_ => throw IllegalStateException("nope"))

  test("Calls: the conformance body over the JVM's own module") {
    FacadeConformance.calls(jvm, "echo", "boom")
  }

  test("Calls: the same body over a test's own module type") {
    FacadeConformance.calls(EchoModule("t"), "echo", "boom")
  }

  test("a capability a module type does not give is a compile error, not a refusal") {
    assert(compileErrors("summon[Calls[Mute]]").nonEmpty)
    assert(compileErrors("summon[Speaks[Mute]]").nonEmpty)
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
