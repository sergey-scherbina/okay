package okay.freer

import Freer.*

/** specs/freer-min.md: a handler is a loop over head forms that lives anywhere — inside a delimiter too — and
 * lets a capture through; a capture crossing it is multi-shot, the handler's state in its closure */
class TestHandlers extends okay.testkit.Munit.Diagnosed:
  enum Counter[+A]:
    case Next extends Counter[Int]

  /** a local handler inside a delimiter: answers `Counter.Next` with a running count kept in its closure, passes
   * anything else on. A capture going through is at the delimiter's row, which the INDEX says (`Lvl[D]`), and
   * the handler asks that row to be within what it leaves, `D[A] <: G[A]` — so the capture passes with no claim.
   * THE ONE CLAIM left, in `answer`: the rest `G` holds no `Counter` of its own, so a `Counter` by class is ours at
   * our `X`, anything else is `G`'s (what master proves with `Distinct[E + F]`, Row.scala) */
  def counting[G[+_], D[+A] <: G[A], Dd <: Tuple, S, R, I <: Tuple, O <: Tuple, A](n: Int)(p: Freer[Counter + G, At[D, Dd, S] *: I, At[D, Dd, R] *: O, A])
    : Freer[G, At[D, Dd, S] *: I, At[D, Dd, R] *: O, A] =
    delay:
      val head: Freer[Counter + G, At[D, Dd, S] *: I, At[D, Dd, R] *: O, A] = Machine.run(p)
      head match
        case Return(a) => pure(a)
        case Bind(h, k) => h match
          case Inject(op) => answer(n)(op, k)
          case s: Shift0[d, ?, ?, ?, ?, ?, x] => Bind(s, (v: x) => counting(n)(k(v)))
          case other => fail(s"not a head form: $other")
        case other => fail(s"not a head form: $other")

  def answer[G[+_], D[+A] <: G[A], Dd <: Tuple, S, T, I <: Tuple, O <: Tuple, X, A](n: Int)(op: Counter[X] | G[X], k: X => Freer[Counter + G, At[D, Dd, S] *: I, At[D, Dd, T] *: O, A])
    : Freer[G, At[D, Dd, S] *: I, At[D, Dd, T] *: O, A] = op match
    case c: Counter[X @unchecked] => c match
      case Counter.Next => counting(n + 1)(k(n))
    case other: G[X] @unchecked => Bind(Inject(other), (x: X) => counting(n)(k(x)))

  def value[A](p: Top[Pure, A]): A =
    val head: Top[Pure, A] = Machine.run(p)
    head match
      case Return(a) => a
      case other => fail(s"not a value: $other")


  test("a handler inside a delimiter; a capture crosses it and is resumed twice, each time with the handler's count"):
    val prog: Top[Pure, Int] = reset[Pure, Int](
      counting(0)(
        for
          a <- inject(Counter.Next)
          x <- shift0[Int](k => k(1).flatMap(y => k(y)))
          b <- inject(Counter.Next)
        yield a + b + x))
    // first resumption: a = 0 captured, x = 1, b = 1 → 2; second, k(2): a = 0, x = 2, b = 1 → 3
    assertEquals(value(prog), 3)
