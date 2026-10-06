package okay.freer

import Freer.*

/** specs/freer-min.md: effects and the delimiter stack — a handler is a loop over head forms, polymorphic in the
 * stack, that passes a capture through; so it lives INSIDE a delimiter, and a capture crosses it, many times */
class TestHandlers extends okay.testkit.Munit.Diagnosed:
  enum Counter[+A]:
    case Next extends Counter[Int]
  enum Ask[+A]:
    case Number extends Ask[Int]

  /** a local handler: answers `Counter.Next` with a running count kept in its closure, so a capture that goes
   * through it is resumed with the count it had — multi-shot by construction. It lets a capture through because
   * `Within[Σ, G]` says every delimiter around it has a row inside `G`: the capture's node widens to `G` by the
   * delimiter's own `Widen`, no cast */
  def counting[G[_, _, +_], Σ <: Tuple, S, R, A](n: Int)(p: Freer[Diag[Counter] + G, Σ, S, R, A])(using w: Within[Σ, G]): Freer[G, Σ, S, R, A] =
    delay:
      val head: Freer[Diag[Counter] + G, Σ, S, R, A] = Machine.run(p)
      head match
        case Return(a) => pure(a)
        case Bind(h, k) => (h: @unchecked) match
          case Inject(op) => answer(n)(op, k)
          // a capture going through: the head of the stack is its delimiter, and `Within` has that delimiter's `Widen`
          case s: Shift0[h, o, sp, t, r, x, y] => w match
            case Within.Head(wd, _) => Bind(wd(s: Freer[h, Entry[h, sp, y] *: o, t, r, x]), (v: x) => counting(n)(k(v)))
        case other => fail(s"not a head form: $other")

  /** one operation: ours answered with the count, another's passed on. THE ONE CLAIM of a handler over a row, in
   * both branches: the rest `G` holds no `Counter` of its own, so a `Counter` by class is ours at our `X` and
   * anything else is `G`'s — what master proves with `Distinct[E + F]` (Row.scala), stated here in one place */
  def answer[G[_, _, +_], Σ <: Tuple, S, R, T, X, A](n: Int)(op: Counter[X] | G[T, T, X], k: X => Freer[Diag[Counter] + G, Σ, S, T, A])
                                                   (using Within[Σ, G]): Freer[G, Σ, S, T, A] = op match
    case c: Counter[X @unchecked] => c match
      case Counter.Next => counting(n + 1)(k(n))
    case other: G[T, T, X] @unchecked => Bind(Inject(other), (x: X) => counting(n)(k(x)))

  def value[A](p: Freer[Pure, EmptyTuple, Unit, Unit, A]): A =
    val head: Freer[Pure, EmptyTuple, Unit, Unit, A] = Machine.run(p)
    head match
      case Return(a) => a
      case other => fail(s"not a value: $other")

  val d = Delimiter[Pure, Unit, Int]()

  test("a handler inside a delimiter; a capture crosses it and is resumed twice, each time with the handler's count"):
    // counts 0, 1 inside; k twice: 0 + 1 then 0 + 1 again from the captured count → 1 + 1 = 2... measured below
    val prog = reset(d)(
      counting(0)(
        for
          a <- inject(Counter.Next)
          x <- shift0[Int](d)(k => k(1).flatMap(y => k(y)))
          b <- inject(Counter.Next)
        yield a + b + x))
    note(s"answer: ${value(prog)}")
    // first resumption: a = 0 captured, x = 1, b = 1 → 2; second: k(2): a = 0, x = 2, b = 1 → 3
    assertEquals(value(prog), 3)
