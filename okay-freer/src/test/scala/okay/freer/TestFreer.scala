package okay.freer

import scala.annotation.tailrec
import Freer.*

/** specs/freer-min.md: the tree on its own — ternary effects, rows built by flatMap, dispatch without a cast, the
 * answer pair as a type-changing state, the handler's rest */
class TestFreer extends okay.testkit.Munit.Diagnosed:
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum Say[+A]:
    case Line(s: String) extends Say[Unit]
  enum Cnt[S, R, +A]:
    case Tick[T]() extends Cnt[T, T, Unit]
  /** type-changing state (Atkey), read `R => (S, A)`: before `R`, after `S` */
  enum PState[S, R, +A]:
    case Get[S]() extends PState[S, S, S]
    case Put[S, T](t: T) extends PState[T, S, Unit]

  type Fx = Diag[Ask] + Diag[Say]
  type Top[G[_, _, +_], A] = Freer[G, EmptyTuple, Unit, Unit, A]

  /** a program with no capture is polymorphic in the delimiter stack */
  def one[Σ <: Tuple]: Freer[Fx, Σ, Unit, Unit, Int] =
    for
      n <- inject(Ask.Number)
      _ <- inject(Say.Line(n.toString))
    yield n + 1

  /** one operation answered: `Inject` gave `T = R`, the pointwise bound gives the dispatch — no cast */
  def step[X, B](op: Ask[X] | Say[X], k: X => Top[Fx, B], in: Int, out: StringBuilder): Top[Fx, B] = op match
    case Ask.Number => k(in)
    case Say.Line(l) => out.append(l).append('\n'): Unit; k(())

  @tailrec final def run[A](p: Top[Fx, A], in: Int, out: StringBuilder): A =
    val head: Top[Fx, A] = Machine.run(p)
    head match
      case Return(a) => a
      case Bind(h, k) => (h: @unchecked) match
        case Inject(op) => run(step(op, k, in, out), in, out)
      case other => fail(s"not handled: $other")

  @tailrec final def runState[S, R, A](p: Freer[PState, EmptyTuple, S, R, A], s: R): (S, A) =
    val head: Freer[PState, EmptyTuple, S, R, A] = Machine.run(p)
    head match
      case Return(a) => (s, a)
      case Bind(Perform(op), k) => op match
        case PState.Get() => runState(k(s), s)
        case PState.Put(t) => runState(k(()), t)
      case other => fail(s"not handled: $other")

  def runPure[A](p: Top[Pure, A]): A =
    val head: Top[Pure, A] = Machine.run(p)
    head match
      case Return(a) => a
      case other => fail(s"not a value: $other")

  test("a for over two signatures builds the union row, and runs"):
    val out = StringBuilder()
    assertEquals(run(one, 41, out), 42)
    assertEquals(out.toString.trim, "41")

  test("(F + G) + F is accepted where F + G is expected; pure is a program of every row"):
    val two: Top[Fx, Int] = one.flatMap(a => inject(Ask.Number).map(_ + a))
    val three: Top[Fx, Int] = pure(3)
    val out = StringBuilder()
    assertEquals(run(two, 41, out), 83)
    assertEquals(run(three, 0, out), 3)

  test("100 000 left-nested maps run in constant stack"):
    val chain = (1 to 100000).foldLeft(pure[Int, EmptyTuple, Unit](0): Top[Fx, Int])((p, _) => p.map(_ + 1))
    assertEquals(run(chain, 0, StringBuilder()), 100000)

  test("type-changing state: the answer index moves Int -> String through Put"):
    val prog: Freer[PState, EmptyTuple, String, Int, Int] =
      for
        s <- perform(PState.Get[Int]())
        _ <- perform(PState.Put[Int, String]((s + 41).toString))
        t <- perform(PState.Get[String]())
      yield t.length
    assertEquals(runState(prog, 1), ("42", 2))

  test("a handler's rest infers positionally"):
    type Rest = Diag[Say] + Cnt
    val three: Top[Diag[Ask] + Rest, Int] =
      for
        n <- inject(Ask.Number)
        _ <- inject(Say.Line("x"))
        _ <- Freer.Inject(Cnt.Tick[Unit]())
      yield n
    def runAsk[G[_, _, +_], A](p: Top[Diag[Ask] + G, A]): Top[G, A] = p.asInstanceOf[Top[G, A]]
    val typed: Top[Rest, Int] = runAsk(three)
    assert(typed.isInstanceOf[Bind[?, ?, ?, ?, ?, ?, ?]])

  test("a delimiter is a node of the tree, in a row with the effects"):
    val d = Delimiter[Fx, Unit, Int]()
    val c: Top[Fx, Int] = reset(d)(one).flatMap(y => inject(Ask.Number).map(_ + y))
    assertEquals(run(c, 1, StringBuilder()), 3)

  test("delay and defer: a million mutual tail calls in constant stack, through the one loop"):
    def even(n: Int): Top[Pure, Boolean] = if n == 0 then pure(true) else delay(odd(n - 1))
    def odd(n: Int): Top[Pure, Boolean] = if n == 0 then pure(false) else defer(even(n - 1))(b => pure(b))
    assertEquals(runPure(even(1000000)), true)
    assertEquals(runPure(odd(1000001)), true)
