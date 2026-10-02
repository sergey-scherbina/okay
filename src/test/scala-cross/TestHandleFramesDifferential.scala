package okay

import okay.Row.*

/**
 * THE FRAME AGREES WITH THE FOLD (handle-frames, specs/handle-frames.md), on every platform.
 *
 * A — one program with no `Shift` in it, handled by `State`'s fast fold, and the same program handled INSIDE a
 * machine (`Shift.run` around it), which steps into the handler's run as its FRAME. Same final state, same value.
 *
 * B — `State` and a multi-shot capture together: since shift-stacked-key and this lane, both orders run the
 * handler as a frame, so the oracle is the answer worked by hand.
 */
class TestHandleFramesDifferential extends munit.FunSuite:

  type S1 = State % Int + Pure

  type SD = Shift % ? + Pure

  /** the same handler run on a machine: `Shift.run` steps into it as a frame */
  def onMachine[A](p: A ! S1): (Int, A) =
    !.run(Shift.run[(Int, A), Pure](State.handle[Int](1)[A, SD](!.widen[A, S1, Shift % ?](p))))

  def agree[A](name: String)(before: => A ! S1, after: A => A ! S1)(using munit.Location): Unit =
    test(name) {
      val fold = !.run(State.handle[Int](1)[A, Pure](before.flatMap(after)))
      assertEquals(onMachine(before.flatMap(after)), fold)
    }

  agree("get, modify, get")(
    State.get[Int].flatMap(a => State.modify[Int](_ * 10).map(_ + a)),
    x => State.get[Int].map(_ + x))

  agree("modify then update: the state threaded through the frame")(
    State.modify[Int](_ + 41),
    x => State.update[Int, Int](s => (s * 2, s + x)))

  def ops(n: Int): Int ! S1 =
    if n == 0 then State.get[Int] else State.modify[Int](_ + n).flatMap(_ => !.tailcall(ops(n - 1)))

  agree("a thousand operations either side")(ops(1000), x => ops(1000).map(_ + x))

  // ---- B: with a capture, worked by hand

  type K = Shift % Int

  test("State INSIDE the reset, a capture resumed twice: each resumption from the state at the capture") {
    // k(v): the state 1 + v, its value; k(1) = 2, k(10) = 11, the clause 2 * 100 + 11
    val p: Int ! Pure = reset[Int, Pure](State.handle[Int](1)[Int, K + Pure](
      for
        x <- shift0[Int, Int, Pure](k => k(1).flatMap(a => k(10).map(b => a * 100 + b))).at[State % Int + K]
        _ <- State.modify[Int](_ + x).at[State % Int + K]
        s <- State.get[Int].at[State % Int + K]
      yield s).map(_._2))
    assertEquals(!.run(p), 211)
  }

  test("State OUTSIDE the reset, a capture resumed twice: the state threads through both in order") {
    // k(1): 1 + 1 = 2, answers 2; k(10): 2 + 10 = 12, answers 12; the clause 2 * 100 + 12; final state 12
    val p: (Int, Int) ! Pure = State.handle[Int](1)[Int, Pure](reset[Int, State % Int](
      for
        x <- shift0[Int, Int, State % Int](k => k(1).flatMap(a => k(10).map(b => a * 100 + b)))
        _ <- State.modify[Int](_ + x).plus[K]
        s <- State.get[Int].plus[K]
      yield s))
    assertEquals(!.run(p), (12, 212))
  }

  // ---- handle-frames-forms: relay and translate, fold against upgraded

  enum Tick[+A] derives Effect:
    case Now(n: Int) extends Tick[Int]

  type T1 = Tick + Pure

  def ticks(n: Int): Int ! T1 =
    if n == 0 then pure(0) else effect[Tick, Int](Tick.Now(n)).at[T1].flatMap(x => !.tailcall(ticks(n - 1)).map(_ * 31 + x))


  /** the answer, and how many operations the clause was asked: a frame that re-ran a part would ask twice */
  def relayIt(p: Int ! T1, machine: Boolean): (Int, Int) =
    var asked = 0
    val g = [X, Y] => (e: Tick[X]) => e match { case Tick.Now(n) => asked += 1; Cont.Pure[X, Y](n * 7) }
    val r =
      if machine then !.run(Shift.run[Int, Pure](Effects.relay[Int, Int, Tick, SD](!.widen[Int, T1, Shift % ?](p))(pure(_))(g)))
      else !.run(Effects.relay[Int, Int, Tick, Pure](p)(pure(_))(g))
    (r, asked)

  def translateIt(p: Int ! T1, machine: Boolean): (Int, Int) =
    var asked = 0
    val r =
      if machine then !.run(Shift.run[Int, Pure](Effects.translate[Int, Tick, SD](!.widen[Int, T1, Shift % ?](p))(
        [X] => (e: Tick[X]) => e match { case Tick.Now(n) => asked += 1; pure[SD, X](n * 7) })))
      else !.run(Effects.translate[Int, Tick, Pure](p)(
        [X] => (e: Tick[X]) => e match { case Tick.Now(n) => asked += 1; pure[Pure, X](n * 7) }))
    (r, asked)

  test("relay: the fold and the frame answer alike, each operation asked once") {
    assertEquals(relayIt(ticks(600), machine = true), relayIt(ticks(600), machine = false))
  }

  test("translate: the fold and the frame answer alike, each operation asked once") {
    assertEquals(translateIt(ticks(600), machine = true), translateIt(ticks(600), machine = false))
  }
