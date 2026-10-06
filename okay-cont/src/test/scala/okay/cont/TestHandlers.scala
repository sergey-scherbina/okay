package okay.cont

import Cont.*

/** specs/freer-min.md: a handler is a delimiter, deep — `k` brings the delimiter, so the handler is in force through
 * a resumption, and a clause may resume more than once */
class TestHandlers extends okay.testkit.Munit.Diagnosed:
  enum Choose[+A]:
    case Flip extends Choose[Boolean]

  /** every way: `Flip` is both, the answers of both resumptions, in order */
  def every[G[+_], A]: Handler[Choose, G, A, List[A]] = new Handler[Choose, G, A, List[A]]:
    def ret(a: A): List[A] = List(a)
    def apply[X, Oc <: Ctx](using o: Oc)(op: Choose[X], k: X => Cont[G, o.Here, o.Here, List[A]]): Cont[G, o.Here, o.Here, List[A]] = op match
      case Choose.Flip => k(true).flatMap(xs => k(false).map(ys => xs ++ ys))

  def value[A](p: Top[Pure, A]): A =
    val head: Top[Pure, A] = Machine.run(p)
    head match
      case Return(a) => a
      case other => fail(s"not a value: $other")

  test("a multi-shot handler: two flips, four worlds, the handler in force in each"):
    val prog: Top[Pure, List[(Boolean, Boolean)]] = handle(every[Pure, (Boolean, Boolean)]):
      for
        a <- perform(Choose.Flip)
        b <- perform(Choose.Flip)
      yield (a, b)
    assertEquals(value(prog), List((true, true), (true, false), (false, true), (false, false)))

  test("a handler inside a delimiter, crossed by a capture: the shift's body, outside the handler, shifts on to the delimiter and resumes twice"):
    // inside `every`: capture out THROUGH the handler to the reset — forwarding by hand, `Perform.forward`'s law:
    // a shift to the handler whose body shifts to the reset and resumes `k1` after — then flip; the reset resumed twice
    val prog: Top[Pure, Int] = reset[Pure, Int]:
      handle(every[Pure, Int]):
        for
          x <- shift0[Int](k1 => shift0[Int](k2 => k2(1).flatMap(a => k2(2).map(b => a + b))).flatMap(k1))
          a <- perform(Choose.Flip)
        yield (if a then 10 else 20) + x
      .map(_.sum)
    // k2(1): the handler's body with x = 1, every world: [11, 21], the reset sums, 32; k2(2): [12, 22], 34; 66
    assertEquals(value(prog), 66)

  test("state answering in place: get and put from the handler's cell, no capture; the last state with the value"):
    val prog: Top[Pure, (Int, String)] = state[Int, Pure, String](1):
      for
        a <- perform(State.Get[Int]())
        _ <- perform(State.Put(a + 41))
        b <- perform(State.Get[Int]())
      yield b.toString
    assertEquals(value(prog), (42, "42"))

  test("state answering in place under a multi-shot handler: ONE cell, the second resumption sees the first's last state"):
    val prog: Top[Pure, (Int, List[Int])] = state[Int, Pure, List[Int]](0):
      handle(every[[X] =>> State[Int, X] | Pure[X], Int]):
        for
          _ <- perform(Choose.Flip)
          n <- perform(State.Get[Int]())
          _ <- perform(State.Put(n + 1))
        yield n
    // true: get 0, put 1; false (resumed from the same flip): get 1, put 2 — no replay of the state
    assertEquals(value(prog), (2, List(0, 1)))
