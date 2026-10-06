package okay.freer

import scala.annotation.tailrec
import Freer.*

object Test:
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum PState[S, R, +A]:
    case Get[S]() extends PState[S, S, S]
    case Put[S, T](t: T) extends PState[T, S, Unit]

  def value[A](p: Freer[Pure, Unit, Unit, A]): A = p.resume match
    case Return(a) => a
    case Inject(op) => op
    case Perform(op) => op
    case Bind(h, _) => (h: @unchecked) match
      case Inject(op) => op
      case Perform(op) => op
    case other => throw IllegalStateException(s"not a value: $other")

  @tailrec def runAsk[A](p: Freer[Diag[Ask], Unit, Unit, A], in: Int): A = p.resume match
    case Return(a) => a
    case Inject(op) => op match
      case Ask.Number => in
    case Bind(h, k) => (h: @unchecked) match
      case Inject(op) => op match
        case Ask.Number => runAsk(k(in), in)
    case other => throw IllegalStateException(s"not handled: $other")

  @tailrec def runState[S, R, A](p: Freer[PState, S, R, A], s: R): (S, A) = p.resume match
    case Return(a) => (s, a)
    case Inject(op) => runState(Bind(Inject(op), (x: A) => Return(x)), s)
    case Perform(op) => runState(Bind(Perform(op), (x: A) => Return(x)), s)
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
    check("k twice: 4")(value(Machine.run(reset(p)(shift0[Int](p)(k => k(1).flatMap(k)).map(_ * 2)))) == 4)
    check("multi-shot: 60")(value(Machine.run(reset(p)(
      shift0[Int](p)(k => k(1).flatMap(a => k(2).flatMap(b => k(3).map(c => a + b + c)))).map(_ * 10)))) == 60)
    check("nested delimiter crossed: 64")(value(Machine.run(
      reset(p)(reset(q)(shift0[Int](p)(k => k(1).flatMap(k)).map(_ + 10)).map(_ * 2)))) == 64)
    // λ$: k carries ret with the delimiter, f(k) never meets ret: k(1) = 3, k(3) = 7, answer 7 (not 5)
    check("dollar: 7")(value(Machine.run(dollar(p)((x: Int) => pure(x + 1))(shift0[Int](p)(k => k(1).flatMap(k)).map(_ * 2)))) == 7)
    check("NoPrompt")(
      try { value(Machine.run(shift0[Int](p)(k => k(1)))); false } catch case e: NoPrompt => e.wanted == "p")
    // answer-type modification realised through PState across shift0, the general forms
    val ps = Prompt[PState, String, Int]("state")
    val atm: Freer[PState, String, Int, Int] =
      Reset(ps, Shift0[PState, String, String, Int, Int, Int](ps, k => perform(PState.Put[Int, String]("s")).flatMap(_ => k(5))).map(_ + 1))
    check("ATM: (s, 6)")(runState(Machine.run(atm), 1) == ("s", 6))
    // an effect inside a delimiter, handed out and answered outside; a for in place, nothing but X named
    val pa = Prompt[Diag[Ask], Unit, Int]("ask")
    val handed = reset(pa)(
      for
        n <- inject(Ask.Number)
        x <- shift0[Int](pa)(k => k(n).flatMap(k))
      yield x + 1)
    check("handed out: 12")(runAsk(Machine.run(handed), 10) == 12)
    // a body wider than the prompt's row is a program over the WIDER row: the shift's k stays at the prompt's
    val wide: Freer[Diag[Ask] + Pure, Unit, Unit, Int] = reset(p)(shift0[Int](p)(k => k(1)).map(_ + 1)).flatMap(x => inject(Ask.Number).map(_ + x))
    check("wider program: 12")(runAsk(Machine.run(wide), 10) == 12)
    def loop(n: Int): Freer[Pure, Unit, Unit, Int] =
      if n == 0 then pure(0) else shift0[Int](p)(k => k(1)).flatMap(x => loop(n - 1).map(_ + x))
    check("100 000 captures, constant stack")(value(Machine.run(reset(p)(loop(100000)))) == 100000)
    var saved: Int => Freer[Pure, Unit, Unit, Int] = null
    check("escape: 0")(value(Machine.run(reset(p)(shift0[Int](p)(k => { saved = k; pure(0) }).map(_ + 1)))) == 0)
    check("escaped k run later: 42")(value(Machine.run(saved(41))) == 42)
    def even(n: Int): Freer[Pure, Unit, Unit, Boolean] = if n == 0 then pure(true) else delay(odd(n - 1))
    def odd(n: Int): Freer[Pure, Unit, Unit, Boolean] = if n == 0 then pure(false) else defer(even(n - 1))(b => pure(b))
    check("delay/defer 1M via resume")(value(even(1000000)))
    check("delay/defer 1M via machine")(value(Machine.run(odd(1000001))))
    val chain = (1 to 100000).foldLeft(pure[Int, Unit](0): Freer[Pure, Unit, Unit, Int])((p, _) => p.map(_ + 1))
    check("100 000 left-nested maps via machine")(value(Machine.run(chain)) == 100000)
