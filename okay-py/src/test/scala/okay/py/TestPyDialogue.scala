package okay.py

import okay.State
import okay.given
import PyValue.*

/**
 * The callback dialogue without python3 (foreign-callbacks; since
 * foreign-one-program it is a direct PROGRAM, `program` then a `continue`
 * per ask): a canned
 * handler plays the Python side — `twice(x)` asks `inc` twice and answers
 * the sum — so the loop's shape is checked in the default gate.
 */
class TestPyDialogue extends munit.FunSuite {

  /** a scripted Python: what it asks and what it answers, in order */
  private final class Script extends okay.Handler[PyEval]:
    var seen = Vector.empty[String]
    private var got = Vector.empty[Long]
    def handle[A](op: PyEval[A]): A = op match
      case PyEval.Program(_, fn, Vector(I64(x)), cbs, _) =>
        seen :+= s"start $fn ${cbs.mkString(",")}"
        Right(PyNode.Perform("inc", Vector(I64(x)), 1, once = true))
      case PyEval.Continue(_, k, Right(I64(v))) =>
        seen :+= s"resume $k $v"
        got :+= v
        if got.size < 2 then Right(PyNode.Perform("inc", Vector(I64(v)), 2, once = true))
        else Right(PyNode.Done(I64(got.sum)))
      case PyEval.Continue(_, k, Left(c)) =>
        seen :+= s"resume $k ${c.kind}"
        Left(c)
      case other => throw IllegalArgumentException(s"not scripted: $other")

  private val inc = Py.callback[Long, Long]("inc")(x => State.modify[Int](_ + 1).map(_ => x + 1))

  test("start, an ask per callback, a resume per answer, then the answer") {
    val py = Script()
    val prog = Py.fn[Long]("m:twice").calling(Py.callbacks(inc))(5L)
    assertEquals(State.handle(0)(prog).runWith(using py), (2, Right(13L)))
    assertEquals(py.seen, Vector("start m:twice inc", "resume 1 6", "resume 2 7"))
  }

  test("an ask for a callback nobody offered is answered with a condition, not run") {
    val py = Script()
    val other = Py.callback[Long, Long]("other")(x => State.modify[Int](_ + 1).map(_ => x))
    val prog = Py.fn[Long]("m:twice").calling(Py.callbacks(other))(5L)
    assertEquals(State.handle(0)(prog).runWith(using py)._1, 0)
    assertEquals(py.seen, Vector("start m:twice other", "resume 1 NoCallback"))
  }
}
