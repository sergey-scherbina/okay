package okay.py

import okay.{!, %, +, Writer, given}
import okay.Row.plus
import PyValue.*

/** the stage's shape without python3 (foreign-streaming): default gate */
class TestPyStreamShape extends munit.FunSuite {

  test("pull-driven: each call happens when its chunk is full, not after the whole source") {
    var produced = 0
    var seenAtCall = Vector.empty[Int]
    val mock = new okay.Handler[PyEval]:
      def handle[A](op: PyEval[A]): A = op match
        case PyEval.Call(_, Vector(Arr(xs)), _) =>
          seenAtCall :+= produced
          Right(Arr(xs))
        case other => throw IllegalArgumentException(s"not scripted: $other")
    // each element PRODUCED only when the source is forced to it: a
    // `Free.delay` per element. (A left-nested `p.flatMap(_ => { produced
    // += 1; tell(x) })` reads one ahead — reassociating the bind runs the
    // next lambda — which is a side effect in a lambda, not a pull.)
    val src: Unit ! Writer % Long + PyEval =
      (1L to 10L).foldRight(okay.pure[Writer % Long, Unit](()))((x, rest) =>
        okay.Free.delay(() => { produced += 1; Writer.tell(x).flatMap(_ => rest) })).plus[PyEval]
    val out = Writer.run(okay.through(src)(Py.stage[Long, Long]("m:id", chunk = 4))).runWith(using mock)._1
    assertEquals(out.toList, (1L to 10L).toList)
    // four produced when the first chunk is sent, eight at the second,
    // and the partial chunk only once the source has ended
    assertEquals(seenAtCall, Vector(4, 8, 10))
  }
}
