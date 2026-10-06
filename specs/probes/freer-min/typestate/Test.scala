package okay.ts

import scala.annotation.tailrec
import Freer.*
import scala.compiletime.testing.typeCheckErrors

object Test:
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum Say[+A]:
    case Line(s: String) extends Say[Unit]

  def value[A](p: Freer[Pure, EmptyTuple, A]): A =
    val head: Freer[Pure, EmptyTuple, A] = Machine.run(p)
    head match
      case Return(a) => a
      case other => throw IllegalStateException(s"not a value: $other")

  @tailrec def runAsk[A](p: Freer[Ask, EmptyTuple, A], in: Int): A =
    val head: Freer[Ask, EmptyTuple, A] = Machine.run(p)
    head match
      case Return(a) => a
      case Bind(Inject(op), k) => op match
        case Ask.Number => runAsk(k(in), in)
      case other => throw IllegalStateException(s"not handled: $other")

  def check(name: String)(cond: => Boolean): Unit =
    val ok: Boolean = try cond catch case e: Throwable => { println(s"  $name: threw $e"); false }
    println((if ok then "ok   " else "FAIL ") + name)

  def main(args: Array[String]): Unit =
    val p = Prompt[Pure, Int]("p")
    val q = Prompt[Pure, Int]("q")
    check("k twice: 4")(value(reset(p)(shift0[Int](p)(k => k(1).flatMap(k)).map(_ * 2))) == 4)
    check("multi-shot: 60")(value(reset(p)(
      shift0[Int](p)(k => k(1).flatMap(a => k(2).flatMap(b => k(3).map(c => a + b + c)))).map(_ * 10))) == 60)
    check("nested delimiter crossed, Has = Tail(Head): 64")(value(
      reset(p)(reset(q)(shift0[Int](p)(k => k(1).flatMap(k)).map(_ + 10)).map(_ * 2))) == 64)
    // the same prompt twice: the inner delimiter is the one (Head before Tail)
    // k(x) = x + 10 (the inner delimiter), k(1) = 11, k(11) = 21, then the outer map: 42 — not 64 (the outer one)
    check("same prompt nested: the innermost delimiter, 42")(value(reset(p)(reset(p)(shift0[Int](p)(k => k(1).flatMap(k)).map(_ + 10)).map(_ * 2))) == 42)
    check("dollar: 7")(value(dollar(p)((x: Int) => pure(x + 1))(shift0[Int](p)(k => k(1).flatMap(k)).map(_ * 2))) == 7)
    val pa = Prompt[Ask, Int]("ask")
    val handed = reset(pa)(
      for
        n <- inject(Ask.Number)
        x <- shift0[Int](pa)(k => k(n).flatMap(k))
      yield x + 1)
    check("handed out and answered outside: 12")(runAsk(handed, 10) == 12)
    // a program written OUTSIDE its reset declares the stack it is for
    def loop(n: Int)(using In[At["p", Freer[Pure, EmptyTuple, Int]] *: EmptyTuple]): Freer[Pure, At["p", Freer[Pure, EmptyTuple, Int]] *: EmptyTuple, Int] =
      if n == 0 then pure(0) else shift0[Int](p)(k => k(1)).flatMap(x => loop(n - 1).map(_ + x))
    check("100 000 captures, constant stack")(value(reset(p)(loop(100000))) == 100000)
    var saved: Int => Freer[Pure, EmptyTuple, Int] = null
    check("escape: 0")(value(reset(p)(shift0[Int](p)(k => { saved = k; pure(0) }).map(_ + 1))) == 0)
    check("escaped k, a program of the outside, run later: 42")(value(saved(41)) == 42)
    def even(n: Int): Freer[Pure, EmptyTuple, Boolean] = if n == 0 then pure(true) else delay(odd(n - 1))
    def odd(n: Int): Freer[Pure, EmptyTuple, Boolean] = if n == 0 then pure(false) else defer(even(n - 1))(b => pure(b))
    check("delay/defer 1M")(value(even(1000000)))
    // STATIC: a capture to a prompt with no delimiter in force does not typecheck
    val errs = typeCheckErrors("""
      val p = Prompt[Pure, Int]("p")
      val bad: Freer[Pure, EmptyTuple, Int] = shift0[Int](p)(k => k(1))""")
    check("shift0 with no reset: a compile error, not NoPrompt")(errs.nonEmpty && errs.exists(_.message.contains("Has[")))
    errs.foreach(e => println("  refused: " + e.message.linesIterator.next()))
    // STATIC: a program that claims a delimiter it has not installed cannot be run
    val errs2 = typeCheckErrors("""
      val p = Prompt[Pure, Int]("p")
      val claims: Freer[Pure, At["p", Freer[Pure, EmptyTuple, Int]] *: EmptyTuple, Int] = shift0[Int](p)(k => k(1))
      Machine.run(claims)""")
    check("a program under a claimed delimiter cannot be run alone")(errs2.nonEmpty)
    errs2.foreach(e => println("  refused: " + e.message.linesIterator.next()))
