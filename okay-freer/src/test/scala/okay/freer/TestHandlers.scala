package okay.freer

import Freer.*

/** specs/freer-min.md: a handler is a loop over head forms that lives anywhere — inside a delimiter too — and
 * lets a capture through; a capture crossing it is multi-shot, the handler's state in its closure */
class TestHandlers extends okay.testkit.Munit.Diagnosed:
  enum Counter[+A]:
    case Next extends Counter[Int]

  /** a local handler: answers `Counter.Next` with a running count kept in its closure, passes anything else on.
   * THE ONE CLAIM of a handler over a row, in one place: the rest `G` holds no `Counter` of its own, so a `Counter`
   * by class is ours at our `X`, anything else is `G`'s — and a capture going through targets a delimiter whose row
   * is within `G` (what master proves with `Distinct[E + F]`, Row.scala) */
  def counting[G[+_], S, R, A](n: Int)(p: Freer[Counter + G, S, R, A]): Freer[G, S, R, A] =
    delay:
      val head: Freer[Counter + G, S, R, A] = Machine.run(p)
      head match
        case Return(a) => pure(a)
        case Bind(h, k) => (h: @unchecked) match
          case Inject(op) => answer(n)(op, k)
          case s: Shift[h0, ?, t, r, x, ?] =>
            Bind((s: Freer[h0, t, r, x]).asInstanceOf[Freer[G, t, r, x]], (v: x) => counting(n)(k(v)))
        case other => fail(s"not a head form: $other")

  def answer[G[+_], S, T, X, A](n: Int)(op: Counter[X] | G[X], k: X => Freer[Counter + G, S, T, A]): Freer[G, S, T, A] = op match
    case c: Counter[X @unchecked] => c match
      case Counter.Next => counting(n + 1)(k(n))
    case other: G[X] @unchecked => Bind(Inject(other), (x: X) => counting(n)(k(x)))

  def value[A](p: Freer[Pure, A, A, A]): A =
    val head: Freer[Pure, A, A, A] = Machine.run(p)
    head match
      case Return(a) => a
      case other => fail(s"not a value: $other")

  val p = Prompt[Pure, Int]("p")

  test("a handler inside a delimiter; a capture crosses it and is resumed twice, each time with the handler's count"):
    val prog: Freer[Pure, Int, Int, Int] = reset(p)(
      counting(0)(
        for
          a <- inject(Counter.Next)
          x <- shift[Int](p)(k => k(1).flatMap(y => k(y)))
          b <- inject(Counter.Next)
        yield a + b + x))
    // first resumption: a = 0 captured, x = 1, b = 1 → 2; second, k(2): a = 0, x = 2, b = 1 → 3
    assertEquals(value(prog), 3)
