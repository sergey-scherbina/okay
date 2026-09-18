import okay.*

/**
 * TWO INDEPENDENT BRANCHES, AND WHAT IS ACTUALLY PARALLEL ABOUT THEM
 * (specs/static-workflow.md, stage 5 — `static-workflow-par`).
 *
 * A durable journal's record says nothing about which question it
 * answers: the driver matches records positionally, and a tagged
 * record would be a second journal format, which the spec puts out of
 * scope. So a `Par` does NOT interleave records — its two branches'
 * answers are recorded left then right, and this suite pins that.
 *
 * What it DOES buy is the half that costs calendar days: both pending
 * questions are known at once, so a front end can put them to finance
 * and to legal on the same morning instead of one after the other.
 * `walk` answers `Standing.Waiting` with both, and the tests below
 * assert the pair, the order, and that the engine still consumes them
 * one at a time.
 */
class TestProcPar extends munit.FunSuite:

  type P = okay.Pure
  type Row = Delim + P

  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_700_000_000_000L,
                                         id = "id-1", dice = 0.25)

  /** two approvals nobody has to wait for in turn */
  val approvals: Wf.Proc[String, String, Unit, (String, String)] =
    Proc.par(Wf.Proc.ask[String, String, Unit](_ => "finance?"),
             Wf.Proc.ask[String, String, Unit](_ => "legal?"))

  def program(using Wf.Asks[String, String, (String, String), P])
  : (String, String) ! Row = Wf.Proc.program(approvals)(())

  def asked(q: Wf.Question[String, String, ?]): String = q match
    case Wf.Question.Ask(a) => a
    case other => fail(s"expected an Ask, got $other")

  // ── the position is a PAIR of paths ──────────────────────────────

  test("a Par with nothing answered is waiting on BOTH questions, in term order"):
    Wf.Proc.walk(approvals)((), Nil) match
      case Right(Wf.Proc.Standing.Waiting(on, accepted)) =>
        assertEquals(on.length, 2, "a Par's position is a pair of paths")
        assertEquals(on.map((at, _) => at.show), Vector("par0", "par1"))
        assertEquals(on.map((_, q) => asked(q)), Vector("finance?", "legal?"))
        assertEquals(accepted, 0)
      case other => fail(s"expected a Waiting on two questions, got $other")

  test("THE JOURNAL IS STILL ORDERED: one answer leaves one question, and it is the right one"):
    // The left branch is answered, so what remains is an ORDINARY
    // wait — one question, an `Asking`, exactly as it would be
    // without the node. This is the assertion that says a `Par` does
    // not interleave records.
    Wf.Proc.walk(approvals)((), List(Right("yes"))) match
      case Right(Wf.Proc.Standing.Asking(at, q, accepted)) =>
        assertEquals(at.show, "par1")
        assertEquals(asked(q), "legal?")
        assertEquals(accepted, 1)
      case other => fail(s"expected one question outstanding, got $other")

  test("both answered: the branches join into a pair, left first"):
    Wf.Proc.walk(approvals)((), List(Right("yes"), Right("ok"))) match
      case Right(Wf.Proc.Standing.Done(v)) => assertEquals(v, ("yes", "ok"))
      case other => fail(s"expected a pair, got $other")

  test("`pending` reads a Waiting and an Asking alike — a caller need not know which"):
    assertEquals(Wf.Proc.walk(approvals)((), Nil).toOption.get.pending.length, 2)
    assertEquals(Wf.Proc.walk(approvals)((), List(Right("yes"))).toOption.get.pending.length, 1)
    assertEquals(Wf.Proc.walk(approvals)((), List(Right("y"), Right("o")))
      .toOption.get.pending.length, 0)

  test("a Par whose other branch asks nothing is an ordinary wait, not a Waiting of one"):
    val lopsided: Wf.Proc[String, String, Unit, (String, Int)] =
      Proc.par(Wf.Proc.ask[String, String, Unit](_ => "finance?"),
               Proc.arr((_: Unit) => 42))
    Wf.Proc.walk(lopsided)((), Nil) match
      case Right(Wf.Proc.Standing.Asking(at, q, _)) =>
        assertEquals(at.show, "par0")
        assertEquals(asked(q), "finance?")
      case other => fail(s"expected a plain Asking, got $other")

  // ── the keystone, over a term that has a Par in it ───────────────

  test("walk and Wf.replay agree on every prefix — the FIRST pending is the engine's"):
    val full = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, (String, String), P](program)))(_ => okay.pure("yes")))._2
    assertEquals(full.length, 2, "two questions, two records")
    for n <- 0 to full.length do
      val prefix = full.take(n)
      val byTerm = Wf.Proc.walk(approvals)((), prefix) match
        case Right(Wf.Proc.Standing.Done(y)) => Right(Left(y.toString))
        case Right(st) => Right(Right(Wf.Proc.tag(st.pending.head._2)))
        case Left(bad) => Left(bad.toString)
      val paused = !.run(Wf.replay[String, String, (String, String), P](program)(prefix))
      val byReplay = paused.finished match
        case Some(y) => Right(Left(y.toString))
        case None => Right(Right(paused.asking.getOrElse(fail("neither done nor asking"))))
      assertEquals(byTerm, byReplay, s"the two readings of a position disagree at $n answers")

  test("the run answers the pair, on the landed engine and with nothing changed in it"):
    val start = !.run(Wf.resumable[String, String, (String, String), P](program))
    val (st, j) = !.run(Wf.drive(start)(q => okay.pure(if q == "finance?" then "yes" else "ok")))
    st match
      case Wf.Step.Done(v) => assertEquals(v, ("yes", "ok"))
      case other => fail(s"expected the drive to finish, it said $other")
    assertEquals(j, List(Right("yes"), Right("ok")))

  // ── the term says so before it runs ──────────────────────────────

  test("leaves names both branches, with the side each is on"):
    val ls = approvals.leaves
    assertEquals(ls.map(_.name), Vector("ask", "ask"))
    assertEquals(ls.map(_.at.show), Vector("par0", "par1"))

  test("the picture forks and joins"):
    val m = approvals.mermaid()
    assert(m.contains("""{{"both"}}"""), s"no fork in\n$m")
    // both branches leave the fork and both reach the join
    assertEquals(m.linesIterator.count(_.contains("--> q")), 2, m)
    val joins = m.linesIterator.map(_.trim).collect:
      case l if l.endsWith("(( ))") => l.dropRight(5)
    .filterNot(id => id == "s0" || id == "e0").toVector
    assertEquals(joins.length, 1, s"a Par has ONE join, and it is drawn\n$m")
    assertEquals(m.linesIterator.count(_.trim.endsWith(s"--> ${joins.head}")), 2,
      s"both branches must reach the join\n$m")

  test("render indents a Par's branches under it"):
    assertEquals(approvals.render(), "par\n  ask\n  ask\n")

  test("the position marks the branch it is in"):
    val at = Wf.Proc.walk(approvals)((), List(Right("yes"))) match
      case Right(Wf.Proc.Standing.Asking(at, _, _)) => at
      case other => fail(s"expected an Asking, got $other")
    assert(approvals.render(Some(at)).contains("  ask  <-- here"), approvals.render(Some(at)))

  // ── and it is an ordinary node of the arrow ──────────────────────

  test("foldMap runs both branches, left first, and pairs what they answer"):
    import Tick.given
    val both: Proc[Tick, Int, (Int, Int)] =
      Proc.par(Proc.op("add")((x: Int) => Tick.Add(x)), Proc.op("add")((x: Int) => Tick.Add(x)))
    // one counter behind both: 0 + 5 = 5, then 5 + 5 = 10 — so the
    // pair also says WHICH RAN FIRST, which a `Par` fixes on purpose
    assertEquals(both.foldMap(Tick.run)(5)(0), (10, (5, 10)))
