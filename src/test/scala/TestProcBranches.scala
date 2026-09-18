import okay.*
import okay.Direct.*
import scala.language.implicitConversions

/**
 * BRANCHES AND LOOPS IN THE NOTATION (proc-notation-branches, v1.1 of
 * specs/proc-notation.md).
 *
 * v1 refused both and named the node that would take them. These are
 * those nodes: an `if` whose branches ask questions is `OnRight` with
 * both branches compiled at the SAME environment, and a `while` is
 * `Iter` with the test in front of the body.
 *
 * The property that makes the loop honest is that nothing mutates: an
 * assignment REBUILDS the environment with one slot replaced, so the
 * value that goes round the loop travels on the arrow's edge and a
 * replay re-derives it exactly.
 */
class TestProcBranches extends munit.FunSuite:

  type P = okay.Pure
  type Row = Delim + P
  type Sig = Wf.Asked[String, String]

  def ask(q: String): Wf.Question[String, String, String] = Wf.Question.Ask(q)
  def patch(id: String): Wf.Question[String, String, Boolean] = Wf.Question.Patched(id)

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

  // ── the branch ───────────────────────────────────────────────────

  /** an `if` whose BRANCHES ask questions — v1 refused this */
  val districted: Wf.Proc[String, String, Unit, String] =
    Proc.direct[Sig, Unit, String]: _ =>
      val city = !ask("city?")
      if city == "Kyiv" then
        val d = !ask("which district?")
        s"$city/$d"
      else s"$city/whole"

  test("both branches are IN the term, and `leaves` reports both"):
    assertEquals(districted.leaves.map(_.name), Vector("ask", "ask"),
      "a branch that was not taken is still part of the program")

  test("only the taken branch ASKS"):
    val (a, askedA, _) = run(districted)(q => if q == "city?" then "Kyiv" else "Podil")
    assertEquals(a, "Kyiv/Podil")
    assertEquals(askedA, List("city?", "which district?"))
    val (b, askedB, _) = run(districted)(_ => "Lviv")
    assertEquals(b, "Lviv/whole")
    assertEquals(askedB, List("city?"), "the untaken branch asked its question anyway")

  test("the walk folds a branch, and agrees with replay on every prefix"):
    val (_, _, j) = run(districted)(q => if q == "city?" then "Kyiv" else "Podil")
    for n <- 0 to j.length do
      val prefix = j.take(n)
      val byTerm = Wf.Proc.walk(districted)((), prefix) match
        case Right(Wf.Proc.Standing.Done(y)) => Right(Left(y))
        case Right(Wf.Proc.Standing.Asking(_, q, _)) => Right(Right(Wf.Proc.tag(q)))
        case Left(bad) => Left(bad.toString)
      val paused = !.run(Wf.replay[String, String, String, P](
        Wf.Proc.program(districted)(()))(prefix))
      val byReplay = paused.finished match
        case Some(y) => Right(Left(y))
        case None => Right(Right(paused.asking.getOrElse(fail("neither done nor asking"))))
      assertEquals(byTerm, byReplay, s"the two readings disagree at $n answers")

  test("an `if` as a val's right-hand side, with a question in a branch"):
    val p: Wf.Proc[String, String, Unit, String] =
      Proc.direct[Sig, Unit, String]: _ =>
        val city = !ask("city?")
        val extra = if city == "Kyiv" then !ask("district?") else "none"
        s"$city:$extra"
    assertEquals(p.leaves.map(_.name), Vector("ask", "ask"))
    assertEquals(run(p)(q => if q == "city?" then "Kyiv" else "Podil")._1, "Kyiv:Podil")
    assertEquals(run(p)(_ => "Lviv")._1, "Lviv:none")

  test("an `if` whose CONDITION is a question, with questions in the branches"):
    val p: Wf.Proc[String, String, Unit, String] =
      Proc.direct[Sig, Unit, String]: _ =>
        val city = !ask("city?")
        if !patch("promo") then
          val code = !ask("promo code?")
          s"$city/$code"
        else city
    assertEquals(p.leaves.map(_.name), Vector("ask", "patch", "ask"))
    val (v, asked, _) = run(p)(q => if q == "city?" then "Kyiv" else "X1")
    assertEquals(v, "Kyiv/X1")
    assertEquals(asked, List("city?", "promo code?"))

  // ── the loop ─────────────────────────────────────────────────────

  /** the shape Appendix A said a static spine could not have: ask how
   * many nights, then one room per night */
  val rooms: Wf.Proc[String, String, Unit, List[String]] =
    Proc.direct[Sig, Unit, List[String]]: _ =>
      val n = !ask("nights?")
      var got = List.empty[String]
      while got.length < n.toInt do
        got = got :+ !ask(s"room ${got.length + 1}?")
      got

  test("a loop whose trip count is an ANSWER"):
    val (v, asked, j) = run(rooms)(q => if q == "nights?" then "3" else q.take(6))
    assertEquals(v, List("room 1", "room 2", "room 3"))
    assertEquals(asked, List("nights?", "room 1?", "room 2?", "room 3?"))
    assertEquals(Wf.Proc.walk(rooms)((), j),
      Right(Wf.Proc.Standing.Done(List("room 1", "room 2", "room 3"))))

  test("the loop's body is ONE leaf in the term, counted once"):
    assertEquals(rooms.leaves.map(_.name), Vector("ask", "ask"),
      "how often the body runs is decided by a value that does not exist yet")

  test("the position inside a loop names the ROUND it is on"):
    val j: Wf.Journal[String] = List(Right("3"), Right("a"), Right("b"))
    Wf.Proc.walk(rooms)((), j) match
      case Right(Wf.Proc.Standing.Asking(at, q, accepted)) =>
        assertEquals(Wf.Proc.tag(q), Right("room 3?"))
        assertEquals(accepted, 3)
        assert(at.show.contains("round2"),
          s"the path must say which turn of the loop it is on, got ${at.show}")
      case other => fail(s"expected to stand inside the loop, got $other")

  test("walk and replay agree on every prefix of the LOOP's journal"):
    val full: Wf.Journal[String] = List(Right("2"), Right("a"), Right("b"))
    for n <- 0 to full.length do
      val prefix = full.take(n)
      val byTerm = Wf.Proc.walk(rooms)((), prefix) match
        case Right(Wf.Proc.Standing.Done(y)) => Right(Left(y))
        case Right(Wf.Proc.Standing.Asking(_, q, _)) => Right(Right(Wf.Proc.tag(q)))
        case Left(bad) => Left(bad.toString)
      val paused = !.run(Wf.replay[String, String, List[String], P](
        Wf.Proc.program(rooms)(()))(prefix))
      val byReplay = paused.finished match
        case Some(y) => Right(Left(y))
        case None => Right(Right(paused.asking.getOrElse(fail("neither done nor asking"))))
      assertEquals(byTerm, byReplay, s"the two readings disagree at $n answers")

  test("a loop that runs ZERO times asks nothing"):
    val (v, asked, _) = run(rooms)(_ => "0")
    assertEquals(v, Nil)
    assertEquals(asked, List("nights?"), "the body ran before the test")

  test("an assignment OUTSIDE a loop rebuilds the environment in place"):
    val p: Wf.Proc[String, String, Unit, String] =
      Proc.direct[Sig, Unit, String]: _ =>
        var acc = "start"
        acc = acc + "/" + (!ask("a?"))
        acc = acc + "/" + (!ask("b?"))
        acc
    assertEquals(p.leaves.map(_.name), Vector("ask", "ask"))
    assertEquals(run(p)(q => q.take(1))._1, "start/a/b")

  // ── what is still refused ────────────────────────────────────────

  test("a question in a `while` CONDITION is refused, and says why"):
    val e = compileErrors("""
      val bad: Wf.Proc[String, String, Unit, String] =
        Proc.direct[Sig, Unit, String] { _ =>
          var s = ""
          while (!ask("more?")) == "yes" do s = s + "x"
          s
        }
    """)
    assert(e.nonEmpty, "a question in a loop's test compiled")
    assert(e.contains("CONDITION"), e)

  test("a question under a `for` is still a mark under a lambda"):
    val e = compileErrors("""
      val bad: Wf.Proc[String, String, Unit, List[String]] =
        Proc.direct[Sig, Unit, List[String]] { _ =>
          List("a", "b").map(x => !ask(x))
        }
    """)
    assert(e.nonEmpty, "a mark under a lambda compiled")
    assert(e.contains("while"), e)
