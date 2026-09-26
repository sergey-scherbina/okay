package okay.r

import okay.Handler
import okay.agent.Durable
import RValue.*
import java.util.concurrent.atomic.AtomicInteger

/**
 * `Durable` journals R calls through `REval`'s own `Journalled`
 * (foreign-journalled, specs/foreign-highlevel.md stage 1). No R needed:
 * a canned handler stands in for it and counts what reached it.
 */
class TestRJournal extends munit.FunSuite {

  private def canned(ran: AtomicInteger): Handler[REval] = new Handler[REval]:
    def handle[A](op: REval[A]): A = op match
      case REval.Call(okay.foreign.Address.Fn(fn), _, _) =>
        ran.incrementAndGet(): Unit
        fn match
          case "stats::median" => Right(Vec(Vector(F64(2.0))))
          case "m::edges" => Right(Vec(Vector(RNull, NA(RType.Integer), NA(RType.Character), F64(Double.NaN))))
          case _ => Left(Condition("simpleError", s"could not find function \"$fn\""))
      case REval.Frame(_, in, _) =>
        ran.incrementAndGet(): Unit
        Right(RFrame(in.cols.map((n, c) => (n, c.reverse))))
      case other => throw IllegalArgumentException(s"not canned: $other")

  private val xs = Vector(Vec(Vector(F64(3), F64(1), F64(2))))

  test("journalled, then replayed from the journal without reaching R") {
    val j = Durable.MemoryJournal()
    val ran = AtomicInteger()
    val live = Durable.over[REval](canned(ran), j)()
    assertEquals(live.handle(REval.Call("stats::median", xs)), Right(Vec(Vector(F64(2.0)))))
    assertEquals(live.handle(REval.Call("nope", Vector.empty)),
      Left(Condition("simpleError", "could not find function \"nope\"")))
    val replay = Durable.replayingOver[REval](j)
    assertEquals(replay.handle(REval.Call("stats::median", xs)), Right(Vec(Vector(F64(2.0)))))
    assertEquals(replay.handle(REval.Call("nope", Vector.empty)),
      Left(Condition("simpleError", "could not find function \"nope\"")))
    assertEquals(ran.get, 2, "replay touches no R")
  }

  test("NULL, typed NA and NaN come back from the journal distinct") {
    val j = Durable.MemoryJournal()
    val _ = Durable.over[REval](canned(ran = AtomicInteger()), j)().handle(REval.Call("m::edges", Vector.empty))
    Durable.replayingOver[REval](j).handle(REval.Call("m::edges", Vector.empty)) match
      case Right(Vec(Vector(RNull, NA(RType.Integer), NA(RType.Character), F64(nan)))) =>
        assert(nan.isNaN, "NaN is not NA")
      case other => fail(s"not the edges back: $other")
  }

  test("a frame round-trips through the journal, and drifted inputs are refused") {
    val j = Durable.MemoryJournal()
    val ran = AtomicInteger()
    val in = RFrame(Vector("x" -> Vector(I32(1), I32(2), NA(RType.Integer))))
    val first = Durable.over[REval](canned(ran), j)().handle(REval.Frame("rev", in, Vector.empty))
    assertEquals(Durable.replayingOver[REval](j).handle(REval.Frame("rev", in, Vector.empty)), first)
    val drifted = RFrame(Vector("x" -> Vector(I32(1), I32(3))))
    val _ = intercept[Durable.Drift](Durable.over[REval](canned(ran), j)().handle(REval.Frame("rev", drifted, Vector.empty)))
    assertEquals(ran.get, 1)
  }
}
