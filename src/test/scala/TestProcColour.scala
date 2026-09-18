import okay.*
import okay.Direct.*
import okay.Proc.given
import scala.language.implicitConversions

/**
 * A BLOCK WITH NO MARKS AT ALL (proc-auto-colour, asked for by the
 * operator the hour `proc-notation-forms` answered "the types are not
 * required either").
 *
 * `direct` has had auto-colouring behind a capability since
 * specs/direct-auto-coloring.md; this is the same device on the arrow
 * road. A question READS AS ITS ANSWER, and the conversion that makes
 * it so requires `Proc.ProcCtx[F]`, which exists only inside a block
 * — so outside one an operation used as a value stays the compile
 * error it always was.
 */
class TestProcColour extends munit.FunSuite:

  type P = okay.Pure
  type Row = Delim + P
  type Sig = Wf.Asked[String, String]

  def ask(q: String): Wf.Question[String, String, String] = Wf.Question.Ask(q)
  def now: Wf.Question[String, String, Long] = Wf.Question.Now()
  def uuid: Wf.Question[String, String, String] = Wf.Question.Uuid()
  def patch(id: String): Wf.Question[String, String, Boolean] = Wf.Question.Patched(id)

  given Wf.Runtime = Wf.Runtime.scripted(millis = 7L, id = "id-1", dice = 0.25)

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

  /** THE BLOCK, with no `!` anywhere */
  val booking: Wf.Proc[String, String, Unit, String] =
    Proc.direct: _ =>
      val city: String = ask("city?")
      val t: Long = now
      s"$city/$t"

  /** the same with marks, for the comparison that matters */
  val marked: Wf.Proc[String, String, Unit, String] =
    Proc.direct: _ =>
      val city = !ask("city?")
      val t = !now
      s"$city/$t"

  test("a block with no marks compiles to the SAME term as one with them"):
    assertEquals(booking.leaves.map(_.name), marked.leaves.map(_.name))
    assertEquals(run(booking)(())(_ => "Kyiv"), run(marked)(())(_ => "Kyiv"))
    assertEquals(run(booking)(())(_ => "Kyiv"), ("Kyiv/7", List("city?")))

  test("the two spellings MIX in one block"):
    val mixed: Wf.Proc[String, String, Unit, String] =
      Proc.direct: _ =>
        val city: String = ask("city?")
        val id = !uuid
        s"$city/$id"
    assertEquals(mixed.leaves.map(_.name), Vector("ask", "uuid"))
    assertEquals(run(mixed)(())(_ => "Kyiv")._1, "Kyiv/id-1")

  test("a coloured question inside an expression is hoisted like a mark"):
    // the first operand IS in an answer's position, because the val is
    // ascribed; the ones after it are not — see the next test
    val p: Wf.Proc[String, String, Unit, String] =
      Proc.direct: _ =>
        val left: String = ask("left?")
        val right: String = ask("right?")
        left + "|" + right
    assertEquals(run(p)(())(q => q.take(1)), ("l|r", List("left?", "right?")))

  test("A QUESTION WHERE `Any` IS ACCEPTED IS REFUSED, not silently stringified"):
    // THE ONE FAILURE AUTO-COLOURING CAN PRODUCE SILENTLY, and the
    // reason this check exists. `String.+` takes `Any`, so nothing
    // asks for the question's ANSWER and the conversion has nothing
    // to fire on: measured before the check, `ask("left?") + "|" +
    // ask("right?")` answered `l|Ask(right?)` and asked ONCE.
    val e = compileErrors("""
      val bad: Wf.Proc[String, String, Unit, String] =
        Proc.direct { _ =>
          val both: String = ask("left?") + "|" + ask("right?")
          both
        }
    """)
    assert(e.nonEmpty, "a question stood where Any is accepted and was quietly stringified")
    assert(e.contains("never asked"), e)

  test("the same shape with a MARK is fine — a mark needs no expected type"):
    val p: Wf.Proc[String, String, Unit, String] =
      Proc.direct: _ =>
        val both: String = (!ask("left?")) + "|" + (!ask("right?"))
        both
    assertEquals(run(p)(())(q => q.take(1)), ("l|r", List("left?", "right?")))

  test("branches and loops colour too"):
    val branchy: Wf.Proc[String, String, Unit, String] =
      Proc.direct: _ =>
        val city: String = ask("city?")
        if patch("promo") then
          val code: String = ask("code?")
          s"$city/$code"
        else city
    assertEquals(branchy.leaves.map(_.name), Vector("ask", "patch", "ask"))
    assertEquals(run(branchy)(())(q => if q == "city?" then "Kyiv" else "X1"),
      ("Kyiv/X1", List("city?", "code?")))

    val rooms: Wf.Proc[String, String, Unit, List[String]] =
      Proc.direct: _ =>
        val n: String = ask("nights?")
        var got = List.empty[String]
        while got.length < n.toInt do
          got = got :+ ask(s"room ${got.length + 1}?")
        got
    assertEquals(run(rooms)(())(q => if q == "nights?" then "2" else q.take(6)),
      (List("room 1", "room 2"), List("nights?", "room 1?", "room 2?")))

  test("the capability is the gate: OUTSIDE a block a question is not a value"):
    val e = compileErrors("""
      val escaped: String = ask("city?")
    """)
    assert(e.nonEmpty, "a question read as its answer outside a Proc.direct block")
