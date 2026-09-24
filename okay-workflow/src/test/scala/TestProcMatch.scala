import okay.*
import okay.Direct.*
import scala.language.implicitConversions

/**
 * A `match` WHOSE CASES ASK QUESTIONS (proc-notation stage 4,
 * proc-notation-case-binders). Paterson's `case`: the pattern's binders
 * join the environment of their branch, every case's questions are in
 * the term, and only the taken case asks.
 */
class TestProcMatch extends munit.FunSuite:

  type P = okay.Pure
  type Sig = Wf.Asked[String, String]

  def ask(q: String): Wf.Question[String, String, String] = Wf.Question.Ask(q)

  given Wf.Runtime = Wf.Runtime.scripted(millis = 1L, id = "id-1", dice = 0.25)

  def done[Q, R](s: Wf.Step[Q, R]): R = s match
    case Wf.Step.Done(r) => r
    case other => fail(s"expected the drive to finish, it said $other")

  def run[R](p: Wf.Proc[String, String, Unit, R])(oracle: String => String)
            : (R, List[String], Wf.Journal[String]) =
    var asked = List.empty[String]
    val (st, j) = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, R, P](Wf.Proc.program(p)(())))): q =>
        asked = asked :+ q
        okay.pure(oracle(q)))
    (done(st), asked, j)

  /** the block's answer is a match: three cases, a guard, binders used AFTER a question */
  val priced: Wf.Proc[String, String, Unit, String] =
    Proc.direct[Sig, Unit, String]: _ =>
      val limit = 100
      val price = !ask("price?")
      price.toIntOption match
        case Some(p) if p > limit =>
          val ok = !ask(s"approve $p?")
          s"big $p $ok"
        case Some(p) =>
          val n = !ask(s"how many at $p?")
          s"total ${p * n.toInt}"
        case None => s"no price: $price"

  test("every case's questions are in the term, in order"):
    assertEquals(priced.leaves.map(_.name), Vector("ask", "ask", "ask"))

  test("only the taken case asks, and its binder is its own"):
    assertEquals(run(priced)(q => if q == "price?" then "50" else "3").take(2),
      ("total 150", List("price?", "how many at 50?")))
    assertEquals(run(priced)(q => if q == "price?" then "500" else "yes").take(2),
      ("big 500 yes", List("price?", "approve 500?")))
    assertEquals(run(priced)(_ => "tea").take(2), ("no price: tea", List("price?")))

  test("the walk folds a match, and agrees with replay on every prefix"):
    for oracle <- List((q: String) => if q == "price?" then "50" else "3",
                       (q: String) => if q == "price?" then "500" else "yes") do
      val (_, _, j) = run(priced)(oracle)
      for n <- 0 to j.length do
        val prefix = j.take(n)
        val byTerm = Wf.Proc.walk(priced)((), prefix) match
          case Right(Wf.Proc.Standing.Done(y)) => Right(Left(y))
          case Right(st) => Right(Right(Wf.Proc.tag(st.pending.head._2)))
          case Left(bad) => Left(bad.toString)
        val paused = !.run(Wf.replay[String, String, String, P](Wf.Proc.program(priced)(()))(prefix))
        val byReplay = paused.finished match
          case Some(y) => Right(Left(y))
          case None => Right(Right(paused.asking.getOrElse(fail("neither done nor asking"))))
        assertEquals(byTerm, byReplay, s"the two readings disagree at $n answers")

  test("a match as a val's right-hand side, several binders from one pattern, read later"):
    val p: Wf.Proc[String, String, Unit, String] =
      Proc.direct[Sig, Unit, String]: _ =>
        val a = !ask("a?")
        val b = !ask("b?")
        val pair = (a.toIntOption, b.toIntOption) match
          case (Some(x), Some(y)) =>
            val op = !ask(s"op for $x and $y?")
            if op == "+" then x + y else x * y
          case (x, _) => x.getOrElse(-1)
        s"$a $b -> $pair"
    assertEquals(p.leaves.map(_.name), Vector("ask", "ask", "ask"))
    assertEquals(run(p)(q => if q == "a?" then "3" else if q == "b?" then "4" else "*")._1, "3 4 -> 12")
    assertEquals(run(p)(q => if q == "a?" then "3" else "x")._1, "3 x -> 3")

  test("a match as a statement of its own: a case that asks for its effect alone"):
    val p: Wf.Proc[String, String, Unit, String] =
      Proc.direct[Sig, Unit, String]: _ =>
        val mode = !ask("mode?")
        mode match
          case "loud" =>
            val _ = !ask("announce!")
          case _ => ()
        mode
    assertEquals(run(p)(_ => "loud").take(2), ("loud", List("mode?", "announce!")))
    assertEquals(run(p)(_ => "quiet").take(2), ("quiet", List("mode?")))

  test("an `if` with a question, as a statement of its own (the position beside the match's)"):
    val p: Wf.Proc[String, String, Unit, String] =
      Proc.direct[Sig, Unit, String]: _ =>
        val mode = !ask("mode?")
        if mode == "loud" then
          val _ = !ask("announce!")
        mode
    assertEquals(run(p)(_ => "loud").take(2), ("loud", List("mode?", "announce!")))
    assertEquals(run(p)(_ => "quiet").take(2), ("quiet", List("mode?")))

  test("a branch's VALUE is how a name is updated: `x = … match …` with questions in the cases"):
    val p: Wf.Proc[String, String, Unit, String] =
      Proc.direct[Sig, Unit, String]: _ =>
        var log = "start"
        log = (!ask("mode?")) match
          case "loud" => log + "/" + (!ask("how loud?"))
          case other => log + "/" + other
        log
    assertEquals(run(p)(q => if q == "mode?" then "loud" else "very")._1, "start/very")
    assertEquals(run(p)(_ => "quiet")._1, "start/quiet")

  test("the same for an `if`: `x = if … then … else …` with a question in a branch"):
    val p: Wf.Proc[String, String, Unit, String] =
      Proc.direct[Sig, Unit, String]: _ =>
        var log = "start"
        log = if (!ask("loud?")) == "yes" then log + "/" + (!ask("how loud?")) else log + "/quiet"
        log + "!"
    assertEquals(run(p)(q => if q == "loud?" then "yes" else "very")._1, "start/very!")
    assertEquals(run(p)(_ => "no")._1, "start/quiet!")

  test("an assignment to an outer name INSIDE a branch is refused by name, for `if` and `match` alike"):
    val e = compileErrors("""
      val bad: Wf.Proc[String, String, Unit, String] =
        Proc.direct[Sig, Unit, String] { _ =>
          var log = "start"
          if (!ask("loud?")) == "yes" then log = log + (!ask("how?")) else log = log + "/quiet"
          log
        }
    """)
    assert(e.contains("is assigned inside a branch"), e)
    val m = compileErrors("""
      val bad: Wf.Proc[String, String, Unit, String] =
        Proc.direct[Sig, Unit, String] { _ =>
          var log = "start"
          (!ask("mode?")) match
            case "loud" => log = log + (!ask("how?"))
            case other => log = other
          log
        }
    """)
    assert(m.contains("is assigned inside a branch"), m)

  test("a question in a case GUARD is refused, and says why"):
    val e = compileErrors("""
      val bad: Wf.Proc[String, String, Unit, String] =
        Proc.direct[Sig, Unit, String] { _ =>
          val a = !ask("a?")
          a match
            case x if (!ask("really?")) == "yes" => x + (!ask("more?"))
            case x => x
        }
    """)
    assert(e.nonEmpty, "a question in a guard compiled")
    assert(e.contains("GUARD"), e)

  test("a match with questions nested inside a larger expression is refused, and says why"):
    val e = compileErrors("""
      val bad: Wf.Proc[String, String, Unit, String] =
        Proc.direct[Sig, Unit, String] { _ =>
          val a = !ask("a?")
          "prefix " + (a match
            case "x" => !ask("x?")
            case y => y)
        }
    """)
    assert(e.nonEmpty, "a nested match with a question compiled")
    assert(e.contains("`match` CASE"), e)
