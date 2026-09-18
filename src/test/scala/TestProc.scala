import okay.*
import okay.Direct.*
import okay.laws.{ArrowLaws, ArrowLawsSuite}
import scala.language.implicitConversions

/**
 * A SIGNATURE WITH A STATE BEHIND IT, so the laws have an effect to
 * be wrong about: a `sample` whose every value is an `arr` satisfies
 * laws a real arrow can break (`TestArrowLaws` says so in its own
 * file, and this is that lesson applied).
 */
enum Tick[+A]:
  case Add(n: Int) extends Tick[Int]

object Tick:
  /** the smallest monad that can SEE a doubled step: a counter */
  type Counting[A] = Int => (Int, A)

  given Monad[Counting] with
    def pure[A](a: A): Counting[A] = s => (s, a)
    extension [A](fa: Counting[A])
      def flatMap[B](f: A => Counting[B]): Counting[B] =
        s =>
          val (s2, a) = fa(s)
          f(a)(s2)

  val run: Tick ==> Counting = [A] => (t: Tick[A]) => t match
    case Add(n) => (s: Int) => (s + n, s + n)

/** `Proc`'s arrow instance against the shared suite — three lines,
 * which is what `arrow-laws` was written to make possible */
class TestProcLaws extends ArrowLawsSuite[[X, Y] =>> Proc[Tick, X, Y]]:
  def laws: ArrowLaws[[X, Y] =>> Proc[Tick, X, Y]] =
    ArrowLaws(Proc.procArrow[Tick], TestProcLaws.sample, TestProcLaws.observe)

object TestProcLaws:
  import Tick.given

  def sample: Proc[Tick, Int, Int] = Proc.op("add")((x: Int) => Tick.Add(x))

  def observe: ArrowLaws.Observe[[X, Y] =>> Proc[Tick, X, Y]] =
    new ArrowLaws.Observe[[X, Y] =>> Proc[Tick, X, Y]]:
      type Out[Y] = Seq[(Int, Y)]
      def run[X, Y](p: Proc[Tick, X, Y], xs: Seq[X]): Seq[(Int, Y)] =
        xs.map(x => p.foldMap(Tick.run)(x)(0))

/**
 * THE DURABLE SPINE, WRITTEN AS A TERM (specs/static-workflow.md
 * stage 1, specs/arrows-plan.md Decision 1).
 *
 * The monadic booking of `TestWf` and the static one below ask the
 * SAME questions in the same order, and the decisive test is that
 * their journals are equal record for record. Everything else here
 * is what only a term can do: name its leaves before it runs, draw
 * itself, and say where it stands without running at all.
 */
class TestProc extends munit.FunSuite:

  type P = okay.Pure
  type Row = Delim + P
  type Sig = Wf.Asked[String, String]

  val A: Optic.Arrow[[X, Y] =>> Proc[Sig, X, Y]] & Optic.Choice[[X, Y] =>> Proc[Sig, X, Y]] =
    Proc.procArrow[Sig]

  extension [X, Y](p: Wf.Proc[String, String, X, Y])
    def >>>[Z](q: Wf.Proc[String, String, Y, Z]): Wf.Proc[String, String, X, Z] =
      A.compose(q, p)

  /** run a step and KEEP what went in: the arrow's answer to a local
   * variable, and the price `proc-notation` exists to remove */
  def keep[X, Y](p: Wf.Proc[String, String, X, Y]): Wf.Proc[String, String, X, (X, Y)] =
    A.arr((x: X) => (x, x)) >>> A.second(p)

  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_700_000_000_000L,
                                         id = "id-1", dice = 0.25)

  def done[Q, R](s: Wf.Step[Q, R]): R = s match
    case Wf.Step.Done(r) => r
    case other => fail(s"expected the drive to finish, it said $other")

  // ── the booking, twice ───────────────────────────────────────────

  /** the STATIC booking: a term, built from combinators */
  val bookingTerm: Wf.Proc[String, String, Unit, String] =
    Wf.Proc.ask[String, String, Unit](_ => "city?") >>>
      keep(Wf.Proc.now) >>>
      A.arr((p: (String, Long)) => p._1) >>>
      keep(Wf.Proc.uuid) >>>
      A.arr((p: (String, String)) => s"${p._1}/${p._2}")

  def bookingStatic(using w: Wf.Asks[String, String, String, P]): String ! Row =
    Wf.Proc.program(bookingTerm)(())

  /** the MONADIC booking, asking the same three questions */
  def bookingMonadic(using w: Wf.Asks[String, String, String, P]): String ! Row = direct:
    val city = !w.pause("city?")
    val _ = !w.now
    val id = !w.uuid
    s"$city/$id"

  test("the static booking runs on the landed engine, and answers"):
    val start = !.run(Wf.resumable[String, String, String, P](bookingStatic))
    val (st, j) = !.run(Wf.drive(start)(_ => okay.pure("Kyiv")))
    assertEquals(done(st), "Kyiv/id-1")
    assertEquals(j, List(Right("Kyiv"), Left(Wf.SysA.Millis(1_700_000_000_000L)),
      Left(Wf.SysA.Text("id-1"))))

  test("ONE JOURNAL: the static and the monadic booking write the same records"):
    def journalOf(body: Wf.Asks[String, String, String, P] ?=> String ! Row) =
      val start = !.run(Wf.resumable[String, String, String, P](body))
      !.run(Wf.drive(start)(_ => okay.pure("Kyiv")))._2
    assertEquals(journalOf(bookingStatic), journalOf(bookingMonadic),
      "the two front ends must be interchangeable over one topic")

  test("an existing monadic journal is accepted by the static term"):
    val monadic = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, String, P](bookingMonadic)))(_ => okay.pure("Kyiv")))._2
    assert(Wf.Proc.accepts(bookingTerm)((), monadic),
      "a term that asks the same questions must read the other front end's journal")
    assertEquals(Wf.Proc.walk(bookingTerm)((), monadic),
      Right(Wf.Proc.Standing.Done("Kyiv/id-1")))

  // ── what only a term can do ──────────────────────────────────────

  test("leaves: every operation the term MAY reach, before it runs"):
    assertEquals(bookingTerm.leaves.map(_.name), Vector("ask", "now", "uuid"))

  test("leaves reports BOTH sides of a choice and an Iter's body ONCE"):
    // `Optic.Choice`'s primitive is `right` (a prism's requirement),
    // so the other side is reached by swapping — which is the mirror
    // the literature calls `left`, written out
    val branch: Wf.Proc[String, String, Either[Unit, Unit], Either[String, String]] =
      A.right(Wf.Proc.ask[String, String, Unit](_ => "right?")) >>>
        A.arr((e: Either[Unit, String]) => e.swap) >>>
        A.right(Wf.Proc.ask[String, String, Unit](_ => "left?")) >>>
        A.arr((e: Either[String, String]) => e.swap)
    assertEquals(branch.leaves.map(_.name), Vector("ask", "ask"),
      "which side runs is decided by a value that does not exist yet")
    val loop: Wf.Proc[String, String, Int, String] =
      Proc.iter(Wf.Proc.ask[String, String, Int](i => s"room $i?") >>>
        A.arr((r: String) => if r == "done" then Right(r) else Left(0)))
    assertEquals(loop.leaves.map(_.name), Vector("ask"),
      "how often the body runs is decided by a value too")

  test("render draws the term, and marks a position"):
    val drawn = bookingTerm.render()
    assert(drawn.contains("ask") && drawn.contains("now") && drawn.contains("uuid"), drawn)
    val at = Wf.Proc.walk(bookingTerm)((), Nil) match
      case Right(Wf.Proc.Standing.Asking(p, _, _)) => p
      case other => fail(s"expected a standing question, got $other")
    assert(bookingTerm.render(Some(at)).contains("<-- here"))

  // ── the keystone: two derivations of one position ────────────────

  test("walk and Wf.replay agree on EVERY prefix of the journal"):
    val full = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, String, P](bookingStatic)))(_ => okay.pure("Kyiv")))._2
    for n <- 0 to full.length do
      val prefix = full.take(n)
      val byTerm = Wf.Proc.walk(bookingTerm)((), prefix) match
        case Right(Wf.Proc.Standing.Done(y)) => Right(Left(y))
        case Right(Wf.Proc.Standing.Asking(_, q, _)) => Right(Right(Wf.Proc.tag(q)))
        case Left(bad) => Left(bad.toString)
      val paused = !.run(Wf.replay[String, String, String, P](bookingStatic)(prefix))
      val byReplay = paused.finished match
        case Some(y) => Right(Left(y))
        case None => Right(Right(paused.asking.getOrElse(fail("neither done nor asking"))))
      assertEquals(byTerm, byReplay, s"the two readings of a position disagree at $n answers")

  test("walk counts the records it accepted, and stops where the journal runs out"):
    val full = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, String, P](bookingStatic)))(_ => okay.pure("Kyiv")))._2
    Wf.Proc.walk(bookingTerm)((), full.take(2)) match
      case Right(Wf.Proc.Standing.Asking(_, q, accepted)) =>
        assertEquals(Wf.Proc.tag(q), Left(Wf.Sys.Uuid))
        assertEquals(accepted, 2)
      case other => fail(s"expected to stand at uuid, got $other")

  // ── the loop: shape that depends on an answer ────────────────────

  /** ask how many nights, then one room per night — the shape
   * Appendix A said a static spine could not have, which is what
   * `Iter` is for */
  val rooms: Wf.Proc[String, String, Unit, List[String]] =
    val askNights = Wf.Proc.ask[String, String, Unit](_ => "nights?") >>>
      A.arr((n: String) => (n.toInt, List.empty[String]))
    val body: Wf.Proc[String, String, (Int, List[String]), Either[(Int, List[String]), List[String]]] =
      A.arr((s: (Int, List[String])) => s) >>>
        keep(Wf.Proc.ask[String, String, (Int, List[String])](s => s"room ${s._2.length + 1}?")) >>>
        A.arr: (p: ((Int, List[String]), String)) =>
          val (n, got) = p._1
          val next = got :+ p._2
          if next.length >= n then Right(next) else Left((n, next))
    askNights >>> Proc.iter(body)

  def roomsProgram(using w: Wf.Asks[String, String, List[String], P]): List[String] ! Row =
    Wf.Proc.program(rooms)(())

  test("a loop whose trip count is an ANSWER, on the engine and in the walk"):
    var asked = List.empty[String]
    val (st, j) = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, List[String], P](roomsProgram))): q =>
        asked = asked :+ q
        okay.pure(if q == "nights?" then "3" else s"r${asked.length - 1}"))
    assertEquals(done(st), List("r1", "r2", "r3"))
    assertEquals(asked, List("nights?", "room 1?", "room 2?", "room 3?"))
    // and the term agrees, folded over the same journal
    assertEquals(Wf.Proc.walk(rooms)((), j), Right(Wf.Proc.Standing.Done(List("r1", "r2", "r3"))))

  test("the position after two rooms names the ROUND it is on"):
    val j: Wf.Journal[String] = List(Right("3"), Right("a"), Right("b"))
    Wf.Proc.walk(rooms)((), j) match
      case Right(Wf.Proc.Standing.Asking(at, q, accepted)) =>
        assertEquals(Wf.Proc.tag(q), Right("room 3?"))
        assertEquals(accepted, 3)
        assert(at.show.contains("round2"),
          s"the path must say which turn of the loop it is on, got ${at.show}")
      case other => fail(s"expected to stand inside the loop, got $other")

  test("walk and replay agree on every prefix of the LOOP's journal too"):
    val full: Wf.Journal[String] = List(Right("2"), Right("a"), Right("b"))
    for n <- 0 to full.length do
      val prefix = full.take(n)
      val byTerm = Wf.Proc.walk(rooms)((), prefix) match
        case Right(Wf.Proc.Standing.Done(y)) => Right(Left(y))
        case Right(Wf.Proc.Standing.Asking(_, q, _)) => Right(Right(Wf.Proc.tag(q)))
        case Left(bad) => Left(bad.toString)
      val paused = !.run(Wf.replay[String, String, List[String], P](roomsProgram)(prefix))
      val byReplay = paused.finished match
        case Some(y) => Right(Left(y))
        case None => Right(Right(paused.asking.getOrElse(fail("neither done nor asking"))))
      assertEquals(byTerm, byReplay, s"the two readings disagree at $n answers")

  // ── the program is allowed to change ─────────────────────────────

  /** v2: a patch BETWEEN the two questions v1 asked */
  val v2: Wf.Proc[String, String, Unit, String] =
    Wf.Proc.ask[String, String, Unit](_ => "city?") >>>
      keep(Wf.Proc.patch[String, String, String]("promo")) >>>
      keep(Wf.Proc.ask[String, String, (String, Boolean)](_ => "nights?")) >>>
      A.arr: (p: ((String, Boolean), String)) =>
        s"${p._1._1}/${if p._1._2 then "promo" else "plain"}/${p._2}"

  def v2Program(using w: Wf.Asks[String, String, String, P]): String ! Row =
    Wf.Proc.program(v2)(())

  test("a v1 journal takes the OLD branch, and its next answer is not eaten"):
    // written before the patch existed: two author's answers, no decision
    val v1: Wf.Journal[String] = List(Right("Kyiv"), Right("3"))
    assertEquals(Wf.Proc.walk(v2)((), v1), Right(Wf.Proc.Standing.Done("Kyiv/plain/3")),
      "the patch must answer false WITHOUT consuming the record that answers `nights?`")

  test("a fresh run stands AT the patch, and the engine decides it"):
    Wf.Proc.walk(v2)((), List(Right("Kyiv"))) match
      case Right(Wf.Proc.Standing.Asking(_, q, _)) =>
        assertEquals(Wf.Proc.tag(q), Left(Wf.Sys.Patch("promo")))
      case other => fail(s"expected to stand at the patch, got $other")
    val (st, j) = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, String, P](v2Program))): q =>
        okay.pure(if q == "city?" then "Kyiv" else "3"))
    assertEquals(done(st), "Kyiv/promo/3")
    assertEquals(j, List(Right("Kyiv"), Left(Wf.SysA.Flag(true)), Right("3")))

  test("walk and replay agree on the v1 journal too — including the patch"):
    val v1: Wf.Journal[String] = List(Right("Kyiv"), Right("3"))
    for n <- 0 to v1.length do
      val prefix = v1.take(n)
      val byTerm = Wf.Proc.walk(v2)((), prefix) match
        case Right(Wf.Proc.Standing.Done(y)) => Right(Left(y))
        case Right(Wf.Proc.Standing.Asking(_, q, _)) => Right(Right(Wf.Proc.tag(q)))
        case Left(bad) => Left(bad.toString)
      val paused = !.run(Wf.replay[String, String, String, P](v2Program)(prefix))
      val byReplay = paused.finished match
        case Some(y) => Right(Left(y))
        case None => Right(Right(paused.asking.getOrElse(fail("neither done nor asking"))))
      assertEquals(byTerm, byReplay, s"the two readings disagree at $n answers")

  test("a journal the term cannot take is DATA, not a throw"):
    // the deploy check in miniature: an answer of the wrong shape
    val wrong: Wf.Journal[String] = List(Left(Wf.SysA.Millis(1L)))
    Wf.Proc.walk(bookingTerm)((), wrong) match
      case Left(bad) =>
        assertEquals(bad.record, 0)
        assert(bad.why.contains("cannot take"), bad.why)
      case other => fail(s"expected a Stranded, got $other")
    assert(!Wf.Proc.accepts(bookingTerm)((), wrong))
