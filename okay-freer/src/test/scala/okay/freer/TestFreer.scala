package okay.freer

import scala.annotation.tailrec
import Freer.*

/** specs/freer-min.md: the stage-0 probes as a suite — what the four-node tree proves on its own */
class TestFreer extends okay.testkit.Munit.Diagnosed:
  // two unary signatures, as today
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum Say[+A]:
    case Line(s: String) extends Say[Unit]
  // a diagonal three-place signature
  enum Cnt[S, R, +A]:
    case Tick[T]() extends Cnt[T, T, Unit]
  // an index-moving one: type-changing state (Atkey), read `R => (S, A)` — before `R`, after `S`
  enum PState[S, R, +A]:
    case Get[S]() extends PState[S, S, S]
    case Put[S, T](t: T) extends PState[T, S, Unit]

  type Row = Diag[Ask] + Diag[Say]

  val one: Freer[Row, Unit, Unit, Int] =
    for
      n <- inject(Ask.Number)
      _ <- inject(Say.Line(n.toString))
    yield n + 1

  /** one operation answered: the node gave `T = R`, the pointwise bound `G$[T, T, X] <: Ask[X] | Say[X]` gives the
   * dispatch — no cast */
  def step[X, B](op: Ask[X] | Say[X], k: X => Freer[Row, Unit, Unit, B], in: Int, out: StringBuilder)
    : Freer[Row, Unit, Unit, B] = op match
    case Ask.Number => k(in)
    case Say.Line(l) => out.append(l).append('\n'): Unit; k(())

  @tailrec final def run[A](p: Freer[Row, Unit, Unit, A], in: Int, out: StringBuilder): A =
    p.resume match
      case Return(a) => a
      case Inject(op) => run(step(op, (x: A) => Return(x), in, out), in, out)
      case Perform(op) => run(step(op, (x: A) => Return(x), in, out), in, out)
      // `inject` is the only door for a unary operation, so under a `Bind` the head is an `Inject` — the node
      // that gives `T = R`; a `Perform` of a diagonal row would leave `T` unknown, and nothing builds one
      case Bind(h, k) => (h: @unchecked) match
        case Inject(op) => run(step(op, k, in, out), in, out)

  /** the type-changing state run: `Get` keeps the index, `Put` moves it; the GADT match types every step */
  @tailrec final def runState[S, R, A](p: Freer[PState, S, R, A], s: R): (S, A) =
    p.resume match
      case Return(a) => (s, a)
      // a bare operation: one step through the `Bind` case, its continuation the value
      case Inject(op) => runState(Bind(Inject(op), (x: A) => Return(x)), s)
      case Perform(op) => runState(Bind(Perform(op), (x: A) => Return(x)), s)
      case Bind(h, k) => (h: @unchecked) match
        case Inject(op) => op match
          case PState.Get() => runState(k(s), s)
          case PState.Put(t) => runState(k(()), t)
        case Perform(op) => op match
          case PState.Get() => runState(k(s), s)
          case PState.Put(t) => runState(k(()), t)

  /** a program of the empty row has only a value */
  def runPure[A](p: Freer[Pure, Unit, Unit, A]): A =
    p.resume match
      case Return(a) => a
      case Inject(op) => op
      case Perform(op) => op
      case Bind(h, _) => (h: @unchecked) match
        case Inject(op) => op
        case Perform(op) => op

  test("a for over two signatures builds the union row, and runs"):
    val out = StringBuilder()
    assertEquals(run(one, 41, out), 42)
    assertEquals(out.toString.trim, "41")

  test("(F + G) + F is accepted where F + G is expected; pure is a program of every row"):
    val two: Freer[Row, Unit, Unit, Int] = one.flatMap(a => inject(Ask.Number).map(_ + a))
    val three: Freer[Row, Unit, Unit, Int] = pure(3)
    val out = StringBuilder()
    assertEquals(run(two, 41, out), 83)
    assertEquals(run(three, 0, out), 3)

  test("100 000 left-nested maps run in constant stack"):
    val chain = (1 to 100000).foldLeft(pure[Int, Unit](0): Freer[Row, Unit, Unit, Int])((p, _) => p.map(_ + 1))
    assertEquals(run(chain, 0, StringBuilder()), 100000)

  test("type-changing state: the index moves Int -> String through Put"):
    val prog: Freer[PState, String, Int, Int] =
      for
        s <- perform(PState.Get[Int]())
        _ <- perform(PState.Put[Int, String]((s + 41).toString))
        t <- perform(PState.Get[String]())
      yield t.length
    note(s"program: ${prog.getClass.getSimpleName}")
    assertEquals(runState(prog, 1), ("42", 2))

  test("an index-moving signature and unary effects in one row"):
    val mixed: Freer[Row + PState, String, Int, Unit] =
      for
        n <- inject(Ask.Number)
        s <- perform(PState.Get[Int]())
        _ <- perform(PState.Put[Int, String]((s + n).toString))
        _ <- inject(Say.Line("moved"))
      yield ()
    assert(mixed.isInstanceOf[Bind[?, ?, ?, ?, ?, ?]])

  test("a handler's rest infers positionally"):
    type Rest = Diag[Say] + Cnt
    val three: Freer[Diag[Ask] + Rest, Unit, Unit, Int] =
      for
        n <- inject(Ask.Number)
        _ <- inject(Say.Line("x"))
        _ <- Freer.Inject(Cnt.Tick[Unit]())
      yield n
    def runAsk[G[_, _, +_], A](p: Freer[Diag[Ask] + G, Unit, Unit, A]): Freer[G, Unit, Unit, A] = p.asInstanceOf[Freer[G, Unit, Unit, A]]
    val rest = runAsk(three)
    val typed: Freer[Rest, Unit, Unit, Int] = rest
    assert(typed.isInstanceOf[Bind[?, ?, ?, ?, ?, ?]])

  test("Control sits in a row with the effects"):
    val p = Prompt[Unit, Int]("p")
    val c: Freer[Ctl[Row] + Row, Unit, Unit, Int] =
      perform(Control.Reset(p, (x: Int) => pure(x), one)).flatMap(y => inject(Ask.Number).map(_ + y))
    assert(c.isInstanceOf[Bind[?, ?, ?, ?, ?, ?]])

  test("delay and defer: a million mutual tail calls in constant stack"):
    def even(n: Int): Freer[Pure, Unit, Unit, Boolean] = if n == 0 then pure(true) else delay(odd(n - 1))
    def odd(n: Int): Freer[Pure, Unit, Unit, Boolean] = if n == 0 then pure(false) else defer(even(n - 1))(b => pure(b))
    assertEquals(runPure(even(1000000)), true)
    assertEquals(runPure(odd(1000001)), true)
