package okay.k7

import scala.annotation.tailrec
import Freer.*
import scala.compiletime.testing.typeCheckErrors

object Test:
  /** TERNARY effects, ZIO's shape: a unary one declares its operations on the diagonal, polymorphic in the index */
  enum Ask[S, R, +A]:
    case Number[T]() extends Ask[T, T, Int]
  enum Say[S, R, +A]:
    case Line[T](s: String) extends Say[T, T, Unit]
  enum PState[S, R, +A]:
    case Get[S]() extends PState[S, S, S]
    case Put[S, T](t: T) extends PState[T, S, Unit]
  type Top[G[_, _, +_], A] = Freer[G, EmptyTuple, Unit, Unit, A]

  def value[S, A](p: Freer[Pure, EmptyTuple, S, S, A]): A =
    val head: Freer[Pure, EmptyTuple, S, S, A] = Machine.run(p)
    head match
      case Return(a) => a
      case other => throw IllegalStateException(s"not a value: $other")

  /** dispatch on a union of ternary rows: the type test picks the signature, the GADT match on its diagonal
   * operation recovers the middle index `T` and the value — no cast, no Inject */
  def step[T, X, B](op: Ask[T, Unit, X] | Say[T, Unit, X], k: X => Freer[Ask + Say, EmptyTuple, Unit, T, B], in: Int, out: StringBuilder)
    : Top[Ask + Say, B] = op match
    case a: Ask[T, Unit, X] => a match
      case Ask.Number() => k(in)
    case s: Say[T, Unit, X] => s match
      case Say.Line(l) => out.append(l).append('\n'); k(())

  @tailrec def run[A](p: Top[Ask + Say, A], in: Int, out: StringBuilder): A =
    val head: Top[Ask + Say, A] = Machine.run(p)
    head match
      case Return(a) => a
      case Bind(Perform(op), k) => run(step(op, k, in, out), in, out)
      case other => throw IllegalStateException(s"not handled: $other")

  @tailrec def runAsk[A](p: Top[Ask, A], in: Int): A =
    val head: Top[Ask, A] = Machine.run(p)
    head match
      case Return(a) => a
      case Bind(Perform(op), k) => op match
        case Ask.Number() => runAsk(k(in), in)
      case other => throw IllegalStateException(s"not handled: $other")

  @tailrec def runState[S, R, A](p: Freer[PState, EmptyTuple, S, R, A], s: R): (S, A) =
    val head: Freer[PState, EmptyTuple, S, R, A] = Machine.run(p)
    head match
      case Return(a) => (s, a)
      case Bind(Perform(op), k) => op match
        case PState.Get() => runState(k(s), s)
        case PState.Put(t) => runState(k(()), t)
      case other => throw IllegalStateException(s"not handled: $other")

  def check(name: String)(cond: => Boolean): Unit =
    val ok: Boolean = try cond catch case e: Throwable => { println(s"  $name: threw $e"); false }
    println((if ok then "ok   " else "FAIL ") + name)

  def main(args: Array[String]): Unit =
    // rows built by flatMap over ternary effects; dispatch without a cast
    def one[Σ <: Tuple]: Freer[Ask + Say, Σ, Unit, Unit, Int] =
      for
        n <- perform(Ask.Number())
        _ <- perform(Say.Line(n.toString))
      yield n + 1
    val out = StringBuilder()
    check("ternary unary effects: for, dispatch, 42")(run(one, 41, out) == 42 && out.toString.trim == "41")
    val two: Top[Ask + Say, Int] = one.flatMap(a => perform(Ask.Number()).map(_ + a))
    val three: Top[Ask + Say, Int] = pure(3)
    check("(F + G) + F at F + G; pure in any row")(run(two, 41, out) == 83 && run(three, 0, out) == 3)
    val st: Freer[PState, EmptyTuple, String, Int, Int] =
      for
        s <- perform(PState.Get[Int]())
        _ <- perform(PState.Put[Int, String]((s + 41).toString))
        t <- perform(PState.Get[String]())
      yield t.length
    check("type-changing state: (42, 2)")(runState(st, 1) == ("42", 2))
    // control: no prompts; the nearest delimiter, typed by the head of the stack
    type P = Entry[Pure, Unit, Int]
    val d = Delimiter[Pure, Unit, Int]()
    check("k twice: 4")(value(reset(d)(shift0[Int](d)(k => k(1).flatMap(k)).map(_ * 2))) == 4)
    check("multi-shot: 60")(value(reset(d)(shift0[Int](d)(k => k(1).flatMap(a => k(2).flatMap(b => k(3).map(c => a + b + c)))).map(_ * 10))) == 60)
    check("dollar: 7")(value(dollar(d)((x: Int) => pure(x + 1))(shift0[Int](d)(k => k(1).flatMap(k)).map(_ * 2))) == 7)
    // crossing an inner delimiter, DERIVED: two captures to the nearest, the inner put back by dollar
    val crossing = reset(d)(reset(d)(
      shift0[Int](d)(k1 => shift0[Int](d)(k2 =>
        val k: Int => Freer[Pure, EmptyTuple, Unit, Unit, Int] = x => dollar(d)(k2)(k1(x))
        k(1).flatMap(k))).map(_ + 10)).map(_ * 2))
    check("capture across an inner delimiter, derived: 64")(value(crossing) == 64)
    val da = Delimiter[Ask, Unit, Int]()
    val handed = reset(da)(
      for
        n <- perform(Ask.Number[Unit]())
        x <- shift0[Int](da)(k => k(n).flatMap(k))
      yield x + 1)
    check("handed out and answered outside: 12")(runAsk(handed, 10) == 12)
    val atm: Freer[PState, EmptyTuple, String, Int, Int] =
      Reset[PState, EmptyTuple, String, Int, Int](
        Shift0[PState, EmptyTuple, String, String, Int, Int, Int](k => perform(PState.Put[Int, String]("s")).flatMap(_ => k(5))).map(_ + 1))
    check("ATM across shift0: (s, 6)")(runState(atm, 1) == ("s", 6))
    def loop(n: Int)(using In[P *: EmptyTuple]): Freer[Pure, P *: EmptyTuple, Unit, Unit, Int] =
      if n == 0 then pure(0) else shift0[Int](d)(k => k(1)).flatMap(x => loop(n - 1).map(_ + x))
    check("100 000 captures, constant stack")(value(reset(d)(loop(100000))) == 100000)
    var saved: Int => Top[Pure, Int] = null
    check("escape: 0")(value(reset(d)(shift0[Int](d)(k => { saved = k; pure(0) }).map(_ + 1))) == 0)
    check("escaped k run later: 42")(value(saved(41)) == 42)
    def even(n: Int): Top[Pure, Boolean] = if n == 0 then pure(true) else delay(odd(n - 1))
    def odd(n: Int): Top[Pure, Boolean] = if n == 0 then pure(false) else defer(even(n - 1))(b => pure(b))
    check("delay/defer 1M")(value(even(1000000)))
    val errs = typeCheckErrors("""val bad: Freer[Pure, EmptyTuple, Unit, Unit, Int] = shift0[Int](Delimiter[Pure, Unit, Int]())(k => k(1))""")
    check("shift0 with no reset: a compile error")(errs.nonEmpty)
    errs.foreach(e => println("  refused: " + e.message.linesIterator.next()))
