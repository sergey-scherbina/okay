package okay

import okay.Direct.*

/** direct-one-bind-steps (2026-09-27): a direct block's program, as it
 * runs, never has a LEFT-nested pair of binds at its head (the shape
 * `resume` must rotate) for straight-line code, a branch, or a statement
 * whose value is dropped: the emitter already writes one bind a step. The
 * one left-nested case, a loop followed by the rest of its block, is
 * pinned as it is and filed (backlog direct-loop-then-rest). */
class TestDirectShape extends munit.FunSuite {
  type R = State % Int + Writer % String
  type P = [A] =>> A ! R

  /** run by hand, counting heads that are Bind(Bind(..)) before each resume */
  var st = 0
  def onState[X](e: State[Int, X]): X = e match
    case State.Get() => st
    case State.Update(f) => val (b, v) = f(st); st = v; b
  def onWriter[X](e: Writer[String, X]): X = e match
    case Writer.Say(_) => ()
  def answer[X](e: R[X]): X = split[State % Int, Writer % String](e)(onState(_))(onWriter(_))

  def shape[A](p: A ! R): (Int, Int, A) =
    st = 0
    var x: A ! R = p
    var nested = 0
    var steps = 0
    var out: Option[A] = None
    while out.isEmpty do
      x match
        case Free.Bind(Free.Bind(_, _), _) => nested += 1
        case _ => ()
      steps += 1
      (x.resume: @unchecked) match
        case Free.Return(a) => out = Some(a)
        case Free.Inject(e) => out = Some(answer(e))
        case Free.Bind(Free.Inject(e), k) => x = k(answer(e))
    (nested, steps, out.get)

  test("one bind a step: straight-line, a branch, a dropped value; a loop then the rest is left-nested") {
    val straight: Int ! R = direct[P] {
      val a = !State.get[Int]
      !Writer.tell(s"a=$a")
      val b = !State.modify[Int](_ + a + 1)
      a + b
    }
    val branch: Int ! R = direct[P] {
      val a = !State.modify[Int](_ + 1)
      val c = if a > 0 then !State.get[Int] else 0
      !Writer.tell(s"c=$c")
      c + 1
    }
    val discard: Int ! R = direct[P] {
      val _ = !State.set[Int](3)
      val _ = !State.modify[Int](_ * 2)
      val v = !State.get[Int]
      v
    }
    val loop: Int ! R = direct[P] {
      for i <- 1 to 3 do !Writer.tell(s"i=$i")
      !State.get[Int]
    }
    assertEquals(shape(straight), (0, 4, 1))
    assertEquals(shape(branch), (0, 4, 2))
    assertEquals(shape(discard), (0, 4, 6))
    // the loop's own steps are right-nested (foreachLoop recurses), but the
    // block binds the WHOLE loop before the rest: each iteration's head is
    // Bind(Bind(op, loopK), rest). Worth <=1.1x (left-nested-build-cost).
    assertEquals(shape(loop), (3, 4, 0))
  }
}
