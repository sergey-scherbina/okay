package okay

import okay.Row.*

/**
 * THE FRAME AGREES WITH THE FOLD (handle-frames, specs/handle-frames.md), on every platform.
 *
 * A — no `Shift` anywhere, so the fold really is the fold: one program handled by `State`'s fast loop, and the
 * same with a handler nested in the middle of it, where the loop UPGRADES and the rest — its state then
 * included — runs as a frame on the machine. Same final state, same value.
 *
 * B — `State` and a multi-shot capture together: since shift-stacked-key and this lane, both orders run the
 * handler as a frame, so the oracle is the answer worked by hand.
 */
class TestHandleFramesDifferential extends munit.FunSuite:

  type S1 = State % Int + Pure

  /** a handler run with nothing to do: the loop meets it and hands itself to the machine */
  def nested: Unit ! S1 = State.handle[String]("x")[Unit, Pure](pure(())).map(_ => ()).at[S1]

  def agree[A](name: String)(before: => A ! S1, after: A => A ! S1)(using munit.Location): Unit =
    test(name) {
      val fold = !.run(State.handle[Int](1)[A, Pure](before.flatMap(after)))
      val frame = !.run(State.handle[Int](1)[A, Pure](before.flatMap(a => nested.flatMap(_ => after(a)))))
      assertEquals(frame, fold)
    }

  agree("get, modify, get — the upgrade between them")(
    State.get[Int].flatMap(a => State.modify[Int](_ * 10).map(_ + a)),
    x => State.get[Int].map(_ + x))

  agree("the state at the upgrade is the state the frame starts from")(
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

  /** a handler run with nothing to do, inside the Tick row: the form's loop meets it and upgrades */
  def nestedT: Unit ! T1 = State.handle[String]("x")[Unit, Pure](pure(())).map(_ => ()).at[T1]

  /** the answer, and how many operations the clause was asked: a frame that re-ran the program would ask twice */
  def relayIt(p: Int ! T1): (Int, Int) =
    var asked = 0
    val r = !.run(Effects.relay[Int, Int, Tick, Pure](p)(pure(_))(
      [X, Y] => (e: Tick[X]) => e match { case Tick.Now(n) => asked += 1; Cont.Pure[X, Y](n * 7) }))
    (r, asked)

  def translateIt(p: Int ! T1): (Int, Int) =
    var asked = 0
    val r = !.run(Effects.translate[Int, Tick, Pure](p)(
      [X] => (e: Tick[X]) => e match { case Tick.Now(n) => asked += 1; pure[Pure, X](n * 7) }))
    (r, asked)

  test("relay: the fold and the upgraded frame answer alike, each operation asked once") {
    assertEquals(relayIt(ticks(300).flatMap(x => nestedT.flatMap(_ => ticks(300).map(_ + x)))), relayIt(ticks(300).flatMap(x => ticks(300).map(_ + x))))
  }

  test("translate: the fold and the upgraded frame answer alike, each operation asked once") {
    assertEquals(translateIt(ticks(300).flatMap(x => nestedT.flatMap(_ => ticks(300).map(_ + x)))), translateIt(ticks(300).flatMap(x => ticks(300).map(_ + x))))
  }
