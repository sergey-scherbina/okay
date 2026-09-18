import okay.*
import okay.Direct.*
import scala.language.implicitConversions

/**
 * A DURABLE PROCEDURE, WRITTEN AS STRAIGHT-LINE CODE
 * (specs/proc-notation.md, lane 3 of specs/arrows-plan.md).
 *
 * `static-workflow-proc` landed the term and measured the price it
 * charged: every booking needed a `keep` helper, four times, because
 * what a monadic body holds in a local variable a term carries on its
 * edge. This is that price removed — the block below is the monadic
 * booking's text with the doors renamed, and the macro threads the
 * environment.
 */
class TestProcDirect extends munit.FunSuite:

  type P = okay.Pure
  type Row = Delim + P
  type Sig = Wf.Asked[String, String]

  // the doors, as OPERATIONS: what a block marks is a question of the
  // signature, never a `Proc` — see the refusal tests below
  def ask(q: String): Wf.Question[String, String, String] = Wf.Question.Ask(q)
  def now: Wf.Question[String, String, Long] = Wf.Question.Now()
  def uuid: Wf.Question[String, String, String] = Wf.Question.Uuid()
  def patch(id: String): Wf.Question[String, String, Boolean] = Wf.Question.Patched(id)
  def timer(at: Long): Wf.Question[String, String, Unit] = Wf.Question.Timer(at)
  def signal(name: String): Wf.Question[String, String, String] = Wf.Question.Signalled(name)

  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_700_000_000_000L,
                                         id = "id-1", dice = 0.25)

  def done[Q, R](s: Wf.Step[Q, R]): R = s match
    case Wf.Step.Done(r) => r
    case other => fail(s"expected the drive to finish, it said $other")

  /** THE BLOCK — and this is the whole lane: no `keep`, no `>>>`, no
   * tuple projections, and the names are ordinary names */
  val booking: Wf.Proc[String, String, Unit, String] =
    Proc.direct[Sig, Unit, String]: _ =>
      val city = !ask("city?")
      val t = !now
      val id = !uuid
      s"$city/$t/$id"

  /** the monadic twin, asking the same questions in the same order */
  def bookingMonadic(using w: Wf.Asks[String, String, String, P]): String ! Row = direct:
    val city = !w.pause("city?")
    val t = !w.now
    val id = !w.uuid
    s"$city/$t/$id"

  def journalOf(body: Wf.Asks[String, String, String, P] ?=> String ! Row) =
    val start = !.run(Wf.resumable[String, String, String, P](body))
    !.run(Wf.drive(start)(_ => okay.pure("Kyiv")))

  test("a block compiles to a term whose leaves are its questions, in order"):
    assertEquals(booking.leaves.map(_.name), Vector("ask", "now", "uuid"))

  test("ONE JOURNAL: the block and the monadic twin are interchangeable"):
    val (st, j) = journalOf(Wf.Proc.program(booking)(()))
    assertEquals(done(st), "Kyiv/1700000000000/id-1")
    assertEquals(j, journalOf(bookingMonadic)._2,
      "the compiled block must write what the monadic body writes")

  test("the compiled term is a term: `walk` folds it with no runtime"):
    val (_, j) = journalOf(Wf.Proc.program(booking)(()))
    assertEquals(Wf.Proc.walk(booking)((), j),
      Right(Wf.Proc.Standing.Done("Kyiv/1700000000000/id-1")))
    // and it agrees with replay on every prefix, the keystone property
    for n <- 0 to j.length do
      val prefix = j.take(n)
      val byTerm = Wf.Proc.walk(booking)((), prefix) match
        case Right(Wf.Proc.Standing.Done(y)) => Right(Left(y))
        case Right(Wf.Proc.Standing.Asking(_, q, _)) => Right(Right(Wf.Proc.tag(q)))
        // this booking has no `Par`, so it can never stand on two
        // questions at once — but the case has existed since
        // static-workflow-par and a match that ignores it is a
        // warning, which this repository counts as red
        case Right(w: Wf.Proc.Standing.Waiting[?, ?]) =>
          fail(s"a term with no Par stood on ${w.pending.length} questions at $n answers")
        case Left(bad) => Left(bad.toString)
      val paused = !.run(Wf.replay[String, String, String, P](
        Wf.Proc.program(booking)(()))(prefix))
      val byReplay = paused.finished match
        case Some(y) => Right(Left(y))
        case None => Right(Right(paused.asking.getOrElse(fail("neither done nor asking"))))
      assertEquals(byTerm, byReplay, s"the two readings disagree at $n answers")

  test("the ENVIRONMENT is threaded: a later question uses an earlier answer"):
    val chained: Wf.Proc[String, String, Unit, String] =
      Proc.direct[Sig, Unit, String]: _ =>
        val city = !ask("city?")
        val hotel = !ask(s"hotel in $city?")
        s"$city/$hotel"
    var asked = List.empty[String]
    val (st, _) = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, String, P](Wf.Proc.program(chained)(())))): q =>
        asked = asked :+ q
        okay.pure(if q == "city?" then "Kyiv" else "Opera"))
    assertEquals(asked, List("city?", "hotel in Kyiv?"),
      "the second question did not see the first answer")
    assertEquals(done(st), "Kyiv/Opera")

  test("the block's INPUT is a name like any other"):
    val greet: Wf.Proc[String, String, String, String] =
      Proc.direct[Sig, String, String]: who =>
        val where = !ask(s"where is $who?")
        s"$who@$where"
    val (st, _) = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, String, P](Wf.Proc.program(greet)("ada")))
    )(_ => okay.pure("Kyiv")))
    assertEquals(done(st), "ada@Kyiv")

  test("a PURE val between two questions emits no leaf, and the Arrs fold"):
    val mixed: Wf.Proc[String, String, Unit, String] =
      Proc.direct[Sig, Unit, String]: _ =>
        val a = !ask("a?")
        val n = a.length
        val doubled = n * 2
        val b = !ask(s"b after $doubled?")
        s"$a/$b"
    assertEquals(mixed.leaves.map(_.name), Vector("ask", "ask"),
      "a pure val must not become a leaf")

  test("a mark inside an expression is hoisted, left to right"):
    val summed: Wf.Proc[String, String, Unit, String] =
      Proc.direct[Sig, Unit, String]: _ =>
        (!ask("left?")) + "|" + (!ask("right?"))
    var asked = List.empty[String]
    val (st, _) = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, String, P](Wf.Proc.program(summed)(())))): q =>
        asked = asked :+ q
        okay.pure(q.take(1)))
    assertEquals(asked, List("left?", "right?"), "the marks ran out of order")
    assertEquals(done(st), "l|r")

  test("an `if` over bound values is ordinary code inside an Arr"):
    val branchy: Wf.Proc[String, String, Unit, String] =
      Proc.direct[Sig, Unit, String]: _ =>
        val city = !ask("city?")
        val promo = !patch("promo")
        if promo then s"$city/promo" else city
    // the leaf takes the AUTHOR'S name for the door, not the case's
    assertEquals(branchy.leaves.map(_.name), Vector("ask", "patch"))
    val (st, _) = journalOf(Wf.Proc.program(branchy)(()))
    assertEquals(done(st), "Kyiv/promo")

  /** the five-line booking of docs/continuations/23, as a block: the
   * world, the runtime, a run that STOPS, an outside event, and a
   * branch new runs take */
  val fullBooking: Wf.Proc[String, String, Unit, String] =
    Proc.direct[Sig, Unit, String]: _ =>
      val city = !ask("which city?")
      val start = !now
      !timer(start + 24 * 3600 * 1000L)
      val paid = !signal("payment")
      val promo = !patch("promo")
      if promo then s"$city/$paid/promo" else s"$city/$paid"

  test("the five-line booking: every door a workflow has, in one block"):
    assertEquals(fullBooking.leaves.map(_.name),
      Vector("ask", "now", "timer", "signal", "patch"))

  test("a block that SLEEPS stops the drive, at a deadline read from the journal"):
    val p = !.run(Wf.resumable[String, String, String, P](Wf.Proc.program(fullBooking)(())))
    val (st, j) = !.run(Wf.drive(p)(_ => okay.pure("Kyiv")))
    assertEquals(st, Wf.Step.Waiting(Wf.Wait.Until(1_700_000_000_000L + 24 * 3600 * 1000L)),
      "the run must stop at the timer rather than block or finish")
    // and the walk agrees about where it stopped, without running
    Wf.Proc.walk(fullBooking)((), j) match
      case Right(Wf.Proc.Standing.Asking(_, q, _)) =>
        assertEquals(Wf.Proc.tag(q),
          Left(Wf.Sys.Timer(1_700_000_000_000L + 24 * 3600 * 1000L)))
      case other => fail(s"expected to stand at the timer, got $other")

  // ── what an arrow cannot do, refused BY NAME ─────────────────────

  test("a step chosen by a bound value is refused: that is ArrowApply"):
    val e = compileErrors("""
      val step: Proc[Sig, Unit, String] = Proc.arr(_ => "x")
      val bad: Wf.Proc[String, String, Unit, String] =
        Proc.direct[Sig, Unit, String] { _ =>
          val city = !ask("city?")
          !step
        }
    """)
    assert(e.nonEmpty, "a Proc used as a step compiled — the spine is no longer static")
    assert(e.contains("`app`"), e)
    assert(e.contains("monad"), e)

  test("a question in a branch NESTED in a larger expression is refused"):
    // a top-level `if` compiles to `OnRight` since
    // proc-notation-branches — see TestProcBranches. One nested
    // inside a bigger expression still cannot, and the reason is the
    // one that decided the whole feature: it would have to hoist, and
    // a hoisted mark RUNS whether or not its branch is taken.
    val e = compileErrors("""
      val bad: Wf.Proc[String, String, Unit, String] =
        Proc.direct[Sig, Unit, String] { _ =>
          val c = !ask("city?")
          c + (if c == "Kyiv" then !ask("which district?") else c)
        }
    """)
    assert(e.nonEmpty, "a mark in a nested branch compiled — it would have run either way")
    assert(e.contains("not the whole of a statement"), e)

  test("a mark under a lambda keeps direct's own refusal"):
    val e = compileErrors("""
      val bad: Wf.Proc[String, String, Unit, List[String]] =
        Proc.direct[Sig, Unit, List[String]] { _ =>
          List("a", "b").map(x => !ask(x))
        }
    """)
    assert(e.nonEmpty, "a mark under a lambda compiled")
    assert(e.contains("lambda"), e)
