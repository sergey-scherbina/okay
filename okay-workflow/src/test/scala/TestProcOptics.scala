import okay.*
import okay.Proc.given
import scala.language.implicitConversions

/**
 * OPTICS AS THE STATE GLUE (specs/static-workflow.md stage 3).
 *
 * The claim this stage tests is that there is NOTHING TO BUILD. An
 * optic is a function polymorphic in a profunctor, constrained by what
 * it needs — a lens asks for `Strong`, a prism for `Choice` — and
 * `Proc` has both instances since stage 1. So a lens applied to a STEP
 * already type-checks, and what is left is to find out whether it
 * behaves, which is not the same thing.
 *
 * What it buys a durable procedure: a step written against the PART of
 * the state it cares about, dropped into a term whose edge carries the
 * whole. The journal never learns the difference — it holds the step's
 * answers and nothing else, which is the assertion that matters.
 */
class TestProcOptics extends munit.FunSuite:

  type P = okay.Pure
  type Row = Delim + P
  type Sig = Wf.Asked[String, String]

  def ask(q: String): Wf.Question[String, String, String] = Wf.Question.Ask(q)

  given Wf.Runtime = Wf.Runtime.scripted(millis = 7L, id = "id-1", dice = 0.25)

  val A: Optic.Arrow[[X, Y] =>> Proc[Sig, X, Y]] & Optic.Choice[[X, Y] =>> Proc[Sig, X, Y]] =
    Proc.procArrow[Sig]

  def done[Q, R](s: Wf.Step[Q, R]): R = s match
    case Wf.Step.Done(r) => r
    case other => fail(s"expected the drive to finish, it said $other")

  def run[X, R](p: Wf.Proc[String, String, X, R])(x: X)(oracle: String => String)
               : (R, List[String]) =
    var asked = List.empty[String]
    val (st, _) = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, R, P](Wf.Proc.program(p)(x)))): q =>
        asked = asked :+ q
        okay.pure(oracle(q)))
    (done(st), asked)

  // ── the state a booking carries ──────────────────────────────────

  final case class Guest(name: String, city: String)
  final case class Booking(guest: Guest, nights: Int)

  val guest: Lens[Booking, Booking, Guest, Guest] = Lens[Booking](_.guest)
  val city: Lens[Guest, Guest, String, String] = Lens[Guest](_.city)

  /** a step that knows ONLY about a city — no booking, no guest */
  val confirmCity: Wf.Proc[String, String, String, String] =
    Proc.direct: c =>
      val answer: String = ask(s"is $c right?")
      answer

  test("a lens runs a step on the PART and puts the answer back"):
    val step: Wf.Proc[String, String, Booking, Booking] =
      guest.andThen(city)(confirmCity)
    val (out, asked) = run(step)(Booking(Guest("ada", "Kyiv"), 3))(_ => "Lviv")
    assertEquals(asked, List("is Kyiv right?"), "the step did not see the focused part")
    assertEquals(out, Booking(Guest("ada", "Lviv"), 3),
      "the surrounding state must be untouched and the answer put back")

  test("the journal holds the STEP's answers and knows nothing of the whole"):
    val step: Wf.Proc[String, String, Booking, Booking] =
      guest.andThen(city)(confirmCity)
    val (_, j) = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, Booking, P](
        Wf.Proc.program(step)(Booking(Guest("ada", "Kyiv"), 3)))))(_ => okay.pure("Lviv")))
    assertEquals(j, List(Right("Lviv")))
    // and the term folds that journal back to the same whole
    assertEquals(Wf.Proc.walk(step)(Booking(Guest("ada", "Kyiv"), 3), j),
      Right(Wf.Proc.Standing.Done(Booking(Guest("ada", "Lviv"), 3))))

  test("a lens does not disturb `leaves`: the step's questions are the term's"):
    val step: Wf.Proc[String, String, Booking, Booking] =
      guest.andThen(city)(confirmCity)
    assertEquals(step.leaves.map(_.name), confirmCity.leaves.map(_.name))

  // ── a prism: the step runs on ONE variant ────────────────────────

  enum Contact:
    case ByMail(address: String)
    case ByPhone(number: String)

  val byMail: Prism[Contact, Contact, String, String] =
    Prism[Contact, Contact, String, String](
      { case Contact.ByMail(a) => Right(a); case other => Left(other) },
      Contact.ByMail(_))

  val checkAddress: Wf.Proc[String, String, String, String] =
    Proc.direct: a =>
      val answer: String = ask(s"deliver to $a?")
      answer

  test("a prism runs the step on the matching variant"):
    val step: Wf.Proc[String, String, Contact, Contact] = byMail(checkAddress)
    val (out, asked) = run(step)(Contact.ByMail("Main st"))(_ => "Second st")
    assertEquals(asked, List("deliver to Main st?"))
    assertEquals(out, Contact.ByMail("Second st"))

  test("and passes every OTHER variant through with NO question asked"):
    val step: Wf.Proc[String, String, Contact, Contact] = byMail(checkAddress)
    val (out, asked) = run(step)(Contact.ByPhone("555"))(_ => fail("a question was asked"))
    assertEquals(asked, Nil, "the absent case must ask nothing")
    assertEquals(out, Contact.ByPhone("555"))
    // and the journal is empty, so a replay agrees
    assertEquals(Wf.Proc.walk(step)(Contact.ByPhone("555"), Nil),
      Right(Wf.Proc.Standing.Done(Contact.ByPhone("555"))))

  test("`leaves` reports the prism's step even where it will not run"):
    // the over-approximation, again: which variant arrives is decided
    // by a value that does not exist yet, so the question is IN the
    // term whether or not any run asks it
    val step: Wf.Proc[String, String, Contact, Contact] = byMail(checkAddress)
    assertEquals(step.leaves.map(_.name), Vector("ask"))

  test("an optic composes with the arrow, not beside it"):
    // the point of one `Profunctor`: a lens-wrapped step is an
    // ordinary step, so `>>>` takes it without knowing it was one
    val bump: Wf.Proc[String, String, Booking, Booking] =
      A.compose(A.arr((b: Booking) => b.copy(nights = b.nights + 1)),
        guest.andThen(city)(confirmCity))
    val (out, _) = run(bump)(Booking(Guest("ada", "Kyiv"), 3))(_ => "Lviv")
    assertEquals(out, Booking(Guest("ada", "Lviv"), 4))

  // ── stage 4: the picture ─────────────────────────────────────────

  val booking: Wf.Proc[String, String, Unit, String] =
    Proc.direct: _ =>
      val city: String = ask("city?")
      val extra: String =
        if city == "Kyiv" then
          val d: String = ask("district?")
          d
        else ""
      val n: String = ask("nights?")
      s"$city$extra/$n"

  val rooms: Wf.Proc[String, String, Unit, List[String]] =
    Proc.direct: _ =>
      val n: String = ask("nights?")
      var got = List.empty[String]
      while got.length < n.toInt do
        got = got :+ ask(s"room ${got.length + 1}?")
      got

  test("the picture names every leaf, and draws BOTH sides of a choice"):
    val m = booking.mermaid()
    assert(m.startsWith("flowchart TD"), m)
    assertEquals("""q\d+\["ask"\]""".r.findAllIn(m).size, 3,
      s"three questions in the term, three in the picture:\n$m")
    assert(m.contains("""{"which side?"}"""), m)
    // both sides: the taken one and the bypass
    assert(m.contains("|right|") && m.contains("|left|"), m)

  test("a loop is a back edge, not an unrolling"):
    val m = rooms.mermaid()
    assert(m.contains("""{"loop"}"""), m)
    assert(m.contains("|again|"), m)
    assertEquals("""q\d+\["ask"\]""".r.findAllIn(m).size, 2,
      s"the body is drawn ONCE — how often it runs is not a fact the term has:\n$m")

  test("the position is marked, and it comes from `walk` — nothing is replayed"):
    val j: Wf.Journal[String] = List(Right("Lviv"))
    val at = Wf.Proc.walk(booking)((), j) match
      case Right(Wf.Proc.Standing.Asking(p, _, _)) => p
      case other => fail(s"expected a standing question, got $other")
    val m = booking.mermaid(Some(at))
    assert(m.contains("class q"), s"the standing question is not marked:\n$m")
    assertEquals("""class q\d+ here""".r.findAllIn(m).size, 1, m)
    // and an unmarked picture marks nothing
    assert(!booking.mermaid().contains("class q"))

  test("pure steps are NOT drawn"):
    // a term that is all plumbing has a picture with no questions in it
    val plumbing: Wf.Proc[String, String, Int, Int] =
      A.arr((i: Int) => i + 1)
    val m = plumbing.mermaid()
    assertEquals("""q\d+""".r.findAllIn(m).size, 0, m)
    assert(m.contains("s0(( ))") && m.contains("e0(( ))"), m)
