package okay.freer

import scala.annotation.tailrec
import Freer.*

/** specs/freer-min.md: the tree on its own — rows built by flatMap, dispatch without a cast, the handler's rest */
class TestFreer extends okay.testkit.Munit.Diagnosed:
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum Say[+A]:
    case Line(s: String) extends Say[Unit]
  enum Cnt[+A]:
    case Tick extends Cnt[Unit]

  type Fx = Ask + Say

  /** a program with no capture is written for any stack: the index is exact, so one at the top is no body for a delimiter */
  def one[Σ <: Tuple]: Freer[Fx, Σ, Int, Int, Int] =
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
    val chain = (1 to 100000).foldLeft(pure[Int, EmptyTuple, Int](0): Top[Fx, Int])((p, _) => p.map(_ + 1))
    assertEquals(run(chain, 0, StringBuilder()), 100000)

  test("a handler's rest infers positionally"):
    type Rest = Say + Cnt
    val three: Top[Ask + Rest, Int] =
      for
        n <- inject(Ask.Number)
        _ <- inject(Say.Line("x"))
        _ <- inject(Cnt.Tick)
      yield n
    def runAsk[G[+_], A](p: Top[Ask + G, A]): Top[G, A] = p.asInstanceOf[Top[G, A]]
    val typed: Top[Rest, Int] = runAsk(three)
    assert(typed.isInstanceOf[Bind[?, ?, ?, ?, ?, ?, ?]])

  test("a delimiter is a node of the tree, in a row with the effects"):
    val c: Top[Fx, Int] = reset[Fx, Int](one).flatMap(y => inject(Ask.Number).map(_ + y))
    assertEquals(run(c, 1, StringBuilder()), 3)

  test("delay and defer: a million mutual tail calls in constant stack, through the one loop"):
    def even(n: Int): Top[Pure, Boolean] = if n == 0 then pure(true) else delay(odd(n - 1))
    def odd(n: Int): Top[Pure, Boolean] = if n == 0 then pure(false) else defer(even(n - 1))(b => pure(b))
    assertEquals(runPure(even(1000000)), true)
    assertEquals(runPure(odd(1000001)), true)
