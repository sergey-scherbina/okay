package okay

import okay.Logic.*

/**
 * Logic's search as a FRAME (handle-frames-catch): searches nested a hundred thousand deep run on a bounded host
 * stack — the fold below `HandleFrames.Limit`, the search frame on a machine at it — and the rest of a split taken
 * on a machine is a program that goes on wherever it is run.
 */
class TestHandleFramesLogic extends munit.FunSuite:

  type Row = Choose + okay.Pure
  def amb[A](as: A*): A ! Row = effect(Choose(as))

  test("a hundred thousand nested cuts") {
    def nest(n: Int): Int ! Row =
      if n == 0 then amb(1, 2, 3) else pure(()).flatMap(_ => cut[Int, okay.Pure](nest(n - 1)).map(_ + 1))
    assertEquals(!.run(runChoice[Int, okay.Pure](nest(100000))), Seq(100001))
  }

  test("a hundred thousand nested splits, every answer through each (the rests handed out from deep)") {
    def nest(n: Int): Int ! Row =
      if n == 0 then amb(1, 2, 3)
      else pure(()).flatMap(_ => ifte[Int, Int, okay.Pure](nest(n - 1))(a => pure(a + 1))(amb[Int]()))
    assertEquals(!.run(runChoice[Int, okay.Pure](nest(100000))), Seq(100001, 100002, 100003))
  }

  type D = Shift % ? + okay.Pure

  test("on a machine: the split's rest goes on outside it, on a machine of its own") {
    val Some((a, rest)) = !.run(Shift.run[Option[(Int, Int ! Choose + D)], okay.Pure](
      msplit[Int, D](effect[Choose + D, Int](Choose(Seq(1, 2, 3)))))): @unchecked
    assertEquals(a, 1)
    assertEquals(!.run(Shift.run[Seq[Int], okay.Pure](runChoice[Int, D](rest))), Seq(2, 3))
  }

  test("on a machine: a capture in a branch, through the search frame and back") {
    val p = Shift.prompt[Seq[Int]]
    // the row written the other way round is the same row: `+` is a union
    val search: Int ! Choose + D = effect[Shift % ? + Choose + okay.Pure, Int](Choose(Seq(1, 2))).flatMap(x =>
      Shift.shift[Seq[Int], Int, Choose + okay.Pure](p)(k => k(x * 10)))
    val r = !.run(Shift.run[Seq[Int], okay.Pure](Shift.push[Seq[Int], okay.Pure](p)(
      observe[Int, D](5)(search).asInstanceOf[Seq[Int] ! D])))
    assertEquals(r, Seq(10, 20))
  }
