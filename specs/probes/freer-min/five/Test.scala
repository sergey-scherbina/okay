package okay.k5

import scala.annotation.tailrec
import Freer.*
import scala.compiletime.testing.typeCheckErrors

object Test:
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum PState[S, R, +A]:
    case Get[S]() extends PState[S, S, S]
    case Put[S, T](t: T) extends PState[T, S, Unit]

  def value[S, A](p: Freer[Pure, EmptyTuple, S, S, A]): A =
    val head: Freer[Pure, EmptyTuple, S, S, A] = Machine.run(p)
    head match
      case Return(a) => a
      case other => throw IllegalStateException(s"not a value: $other")

  @tailrec def runAsk[A](p: Freer[Diag[Ask], EmptyTuple, Unit, Unit, A], in: Int): A =
    val head: Freer[Diag[Ask], EmptyTuple, Unit, Unit, A] = Machine.run(p)
    head match
      case Return(a) => a
      case Bind(Inject(op), k) => op match
        case Ask.Number => runAsk(k(in), in)
      case other => throw IllegalStateException(s"not handled: $other")

  @tailrec def runState[S, R, A](p: Freer[PState, EmptyTuple, S, R, A], s: R): (S, A) =
    val head: Freer[PState, EmptyTuple, S, R, A] = Machine.run(p)
    head match
      case Return(a) => (s, a)
      case Bind(h, k) => (h: @unchecked) match
        case Inject(op) => op match
          case PState.Get() => runState(k(s), s)
          case PState.Put(t) => runState(k(()), t)
        case Perform(op) => op match
          case PState.Get() => runState(k(s), s)
          case PState.Put(t) => runState(k(()), t)
      case other => throw IllegalStateException(s"not handled: $other")

  def check(name: String)(cond: => Boolean): Unit =
    val ok: Boolean = try cond catch case e: Throwable => { println(s"  $name: threw $e"); false }
    println((if ok then "ok   " else "FAIL ") + name)

  def main(args: Array[String]): Unit =
    val p = Prompt[Pure, Unit, Int]("p")
    val q = Prompt[Pure, Unit, Int]("q")
    check("k twice: 4")(value(reset(p)(shift0[Int](p)(k => k(1).flatMap(k)).map(_ * 2))) == 4)
    check("multi-shot: 60")(value(reset(p)(
      shift0[Int](p)(k => k(1).flatMap(a => k(2).flatMap(b => k(3).map(c => a + b + c)))).map(_ * 10))) == 60)
    check("nested delimiter crossed: 64")(value(reset(p)(reset(q)(shift0[Int](p)(k => k(1).flatMap(k)).map(_ + 10)).map(_ * 2))) == 64)
    check("same prompt nested, the innermost: 42")(value(reset(p)(reset(p)(shift0[Int](p)(k => k(1).flatMap(k)).map(_ + 10)).map(_ * 2))) == 42)
    check("dollar: 7")(value(dollar(p)((x: Int) => pure(x + 1))(shift0[Int](p)(k => k(1).flatMap(k)).map(_ * 2))) == 7)
    val pa = Prompt[Diag[Ask], Unit, Int]("ask")
    val handed = reset(pa)(
      for
        n <- inject(Ask.Number)
        x <- shift0[Int](pa)(k => k(n).flatMap(k))
      yield x + 1)
    check("handed out and answered outside: 12")(runAsk(handed, 10) == 12)
    // ANSWER-TYPE MODIFICATION with STATIC prompts: the shift0 body moves the state Int -> String; the nodes
    // themselves, their indexes named, the witness by hand (Head: the prompt is on top)
    val ps = Prompt[PState, String, Int]("state")
    type E = At["state", Freer[PState, EmptyTuple, String, String, Int]]
    val atm: Freer[PState, EmptyTuple, String, Int, Int] =
      Reset[PState, EmptyTuple, String, Int, Int, "state"](ps,
        Shift0[PState, E *: EmptyTuple, EmptyTuple, String, String, Int, Int, Int, "state"](ps, Has.Head(),
          k => perform(PState.Put[Int, String]("s")).flatMap(_ => k(5))).map(_ + 1))
    check("ATM across shift0 with static prompts: (s, 6)")(runState(atm, 1) == ("s", 6))
    def loop(n: Int)(using In[E0 *: EmptyTuple]): Freer[Pure, E0 *: EmptyTuple, Unit, Unit, Int] =
      if n == 0 then pure(0) else shift0[Int](p)(k => k(1)).flatMap(x => loop(n - 1).map(_ + x))
    check("100 000 captures, constant stack")(value(reset(p)(loop(100000))) == 100000)
    var saved: Int => Freer[Pure, EmptyTuple, Unit, Unit, Int] = null
    check("escape: 0")(value(reset(p)(shift0[Int](p)(k => { saved = k; pure(0) }).map(_ + 1))) == 0)
    check("escaped k, a program of the outside, run later: 42")(value(saved(41)) == 42)
    def even(n: Int): Freer[Pure, EmptyTuple, Unit, Unit, Boolean] = if n == 0 then pure(true) else delay(odd(n - 1))
    def odd(n: Int): Freer[Pure, EmptyTuple, Unit, Unit, Boolean] = if n == 0 then pure(false) else defer(even(n - 1))(b => pure(b))
    check("delay/defer 1M")(value(even(1000000)))
    val errs = typeCheckErrors("""
      val p = Prompt[Pure, Unit, Int]("p")
      val bad: Freer[Pure, EmptyTuple, Unit, Unit, Int] = shift0[Int](p)(k => k(1))""")
    check("shift0 with no reset: a compile error")(errs.nonEmpty && errs.exists(_.message.contains("Has[")))
    val errs2 = typeCheckErrors("""
      val p = Prompt[Pure, Unit, Int]("p")
      val claims: Freer[Pure, At["p", Freer[Pure, EmptyTuple, Unit, Unit, Int]] *: EmptyTuple, Unit, Unit, Int] = pure(1)
      Machine.run(claims)""")
    check("a program under a claimed delimiter cannot be run alone")(errs2.nonEmpty)
  type E0 = At["p", Freer[Pure, EmptyTuple, Unit, Unit, Int]]
