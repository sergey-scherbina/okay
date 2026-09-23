package okay.py

import okay.Handler
import okay.agent.Durable
import PyValue.*
import java.util.concurrent.atomic.AtomicInteger

/**
 * `Durable` journals Python calls through `PyEval`'s own `Journalled`
 * (foreign-journalled, specs/foreign-highlevel.md stage 1). No python3
 * needed: a canned handler IS the Python (the TestRMock precedent), and
 * it counts what reached it, so a replay that touched the world shows.
 */
class TestPyJournal extends munit.FunSuite {

  /** answers by address; counts every call that reached "Python" */
  private def canned(ran: AtomicInteger): Handler[PyEval] = new Handler[PyEval]:
    def handle[A](op: PyEval[A]): A = op match
      case PyEval.Call(fn, args) =>
        ran.incrementAndGet(): Unit
        fn match
          case "statistics:median" => Right(F64(2.0))
          case "m:edges" => Right(Arr(Vector(PyNone, F64(Double.NaN), Bytes(Array[Byte](1, 2)), F64(3.0))))
          case "m:fails" => Left(Condition("ZeroDivisionError", "division by zero"))
          case other => Left(Condition("NameError", s"$other with ${args.size} args"))
      case PyEval.Frame(fn, in, _) =>
        ran.incrementAndGet(): Unit
        Right(PyFrame(in.cols :+ ("n" -> Vector(I64(in.cols.headOption.fold(0)(_._2.size).toLong)))))

  private val xs = Vector(Arr(Vector(F64(3), F64(1), F64(2))))

  test("two calls journalled, then replayed from the journal without reaching Python") {
    val j = Durable.MemoryJournal()
    val ran = AtomicInteger()
    val live = Durable.over[PyEval](canned(ran), j)()
    assertEquals(live.handle(PyEval.Call("statistics:median", xs)), Right(F64(2.0)))
    assertEquals(live.handle(PyEval.Call("m:fails", Vector.empty)), Left(Condition("ZeroDivisionError", "division by zero")))
    assertEquals(ran.get, 2)
    assertEquals(j.all.map(_.op), Vector("statistics:median", "m:fails"))

    val replay = Durable.replayingOver[PyEval](j)
    assertEquals(replay.handle(PyEval.Call("statistics:median", xs)), Right(F64(2.0)))
    assertEquals(replay.handle(PyEval.Call("m:fails", Vector.empty)),
      Left(Condition("ZeroDivisionError", "division by zero")), "a condition replays as the same condition")
    assertEquals(ran.get, 2, "replay touches no Python")
  }

  test("None, NaN, bytes and an integral float come back from the journal as they went in") {
    val j = Durable.MemoryJournal()
    val ran = AtomicInteger()
    val _ = Durable.over[PyEval](canned(ran), j)().handle(PyEval.Call("m:edges", Vector.empty))
    Durable.replayingOver[PyEval](j).handle(PyEval.Call("m:edges", Vector.empty)) match
      case Right(Arr(Vector(PyNone, F64(nan), Bytes(b), F64(three)))) =>
        assert(nan.isNaN, "NaN stays a NaN, not None")
        assertEquals(b.toVector, Vector[Byte](1, 2))
        assertEquals(three, 3.0)
      case other => fail(s"not the edges back: $other")
  }

  test("a frame operation round-trips through the journal") {
    val j = Durable.MemoryJournal()
    val ran = AtomicInteger()
    val in = PyFrame(Vector("x" -> Vector(I64(1), I64(2))))
    val first = Durable.over[PyEval](canned(ran), j)().handle(PyEval.Frame("m:count", in, Vector.empty))
    val again = Durable.replayingOver[PyEval](j).handle(PyEval.Frame("m:count", in, Vector.empty))
    assertEquals(again, first)
    assertEquals(again.map(_.cols.map(_._1)), Right(Vector("x", "n")))
    assertEquals(ran.get, 1)
  }

  test("the fingerprint is the address plus a hash of what was ASKED: drifted inputs are refused") {
    val j = Durable.MemoryJournal()
    val ran = AtomicInteger()
    val _ = Durable.over[PyEval](canned(ran), j)().handle(PyEval.Call("statistics:median", xs))
    assert(j.all.head.fingerprint.startsWith("statistics:median#"), j.all.head.fingerprint)
    assertEquals(j.all.head.fingerprint.length, "statistics:median#".length + 64)
    val other = Vector(Arr(Vector(F64(3), F64(1), F64(9))))
    val _ = intercept[Durable.Drift](Durable.over[PyEval](canned(ran), j)().handle(PyEval.Call("statistics:median", other)))
    assertEquals(ran.get, 1, "a drifting program does not call Python either")
  }

  test("withKey is the identity: a subprocess call has nowhere to carry a key") {
    val call = PyEval.Call("statistics:median", xs)
    assertEquals(summon[okay.codec.Journalled[PyEval]].withKey(call, "k-1"), call)
  }
}
