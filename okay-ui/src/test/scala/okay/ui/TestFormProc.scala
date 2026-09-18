package okay.ui

import scala.annotation.unused

import okay.*
import okay.Proc.given
import scala.language.implicitConversions

/**
 * A FORM AS A DURABLE PROCEDURE (proc-form-consumer).
 *
 * The machinery was proved by `static-workflow`: an optic applies to a
 * step with no new code, the journal holds the step's answers and
 * nothing else, and a run resumes where it stopped. What no core test
 * can answer is whether the TERM READS WELL when the questions are a
 * form's FIELDS rather than a workflow's activities — the only kind of
 * question a consumer answers. So this is a consumer: a signup form as
 * a `Proc`, resumed from its journal, rendered by okay-ui.
 *
 * What it buys a page: the user answers a field, closes the laptop,
 * and comes back to the next field with the earlier answers already
 * in. Not a draft in a session — a POSITION in a program.
 *
 * WHAT IT ANSWERED, and it is the reason `Wf.Proc.asking` now exists:
 * a leaf is drawn by its OPERATION's name, which for a workflow is
 * right (`charge`, `ship`) and for a form is useless, because every
 * field is the same operation. In a `direct` block the name comes from
 * the function the author called, so naming a helper after the field
 * names the picture; where the fields are DATA rather than code — a
 * form derived from a `Schema` — there is nowhere to put one def per
 * field, and the name has to be a parameter.
 */
class TestFormProc extends munit.FunSuite:

  type P = okay.Pure

  given Wf.Runtime = Wf.Runtime.scripted(millis = 7L, id = "id-1", dice = 0.25)

  final case class Signup(name: String, email: String, city: String)

  // ── the form as CODE: a helper per field, and the notation reads
  // straight-line. The picture names the fields because the macro
  // names a leaf after the function that was called.

  // the parameter is the SHAPE the macro reads, not a value any of
  // these bodies wants — `@unused` says so rather than a reader
  // wondering what was forgotten
  private def yourName(@unused u: Unit): Wf.Question[String, String, String] = Wf.Question.Ask("name")
  private def yourEmail(@unused u: Unit): Wf.Question[String, String, String] = Wf.Question.Ask("email")
  private def yourCity(@unused u: Unit): Wf.Question[String, String, String] = Wf.Question.Ask("city")

  val typed: Wf.Proc[String, String, Unit, Signup] =
    Proc.direct: u =>
      val name: String = yourName(u)
      val email: String = yourEmail(u)
      val city: String = yourCity(u)
      Signup(name, email, city)

  // ── the same form as DATA: the fields are a list, which is what a
  // schema-derived form is, and each leaf carries its own name

  private def fields(names: Vector[String]): Wf.Proc[String, String, Unit, Vector[String]] =
    names.foldLeft(Proc.arr[Wf.Asked[String, String], Unit, Vector[String]](_ => Vector.empty)) {
      (acc, n) =>
        val askOne = Proc.alongside(Wf.Proc.asking[String, String, Vector[String]](n)(_ => n))
        val keep = Proc.arr[Wf.Asked[String, String], (Vector[String], String), Vector[String]](
          p => p._1 :+ p._2)
        Proc.andThen(Proc.andThen(acc, askOne), keep)
    }

  val derived: Wf.Proc[String, String, Unit, Vector[String]] =
    fields(Vector("name", "email", "city"))

  private def journalOf(answers: String*): Wf.Journal[String] = answers.toList.map(Right(_))

  private def run[R](p: Wf.Proc[String, String, Unit, R])(j: Wf.Journal[String])
                    : Wf.Step[String, R] =
    val paused = !.run(Wf.replay[String, String, R, P](Wf.Proc.program(p)(()))(j))
    !.run(Wf.advance(paused))._1

  /**
   * THE ANSWER THE CONSUMER CAME FOR, and it is a difference between
   * the two ways of writing the same form.
   *
   * Written as CODE, a leaf is named after the FUNCTION the author
   * called — so the picture says `yourName`, not `name`. That is right
   * for a workflow, where the function IS the operation (`charge`,
   * `ship`), and it means a form written in the notation can only name
   * its fields by naming its helpers after them.
   *
   * Written as DATA — which is what a schema-derived form is — the
   * name is a parameter and the picture says the field. There is
   * nowhere to put one def per field when the fields arrive as a
   * `Vector[String]`, which is why `Wf.Proc.asking` exists.
   */
  test("the leaves are named: after the HELPER in code, after the FIELD in data"):
    val code = typed.mermaid()
    for helper <- Vector("yourName", "yourEmail", "yourCity") do
      assert(code.contains(s""""$helper""""), s"the code picture does not name [$helper]:\n$code")
    val data = derived.mermaid()
    for field <- Vector("name", "email", "city") do
      assert(data.contains(s""""$field""""), s"the data picture does not name [$field]:\n$data")
    // three fields, three questions, either way — the plumbing between
    // them draws nothing, because a picture of an `Arr` is a picture
    // of nothing
    for m <- Vector(code, data) do
      assertEquals("""q\d+\[""".r.findAllIn(m).size, 3, m)

  test("a HALF-FILLED form is a journal: it stands at the next field"):
    run(typed)(journalOf("Ada", "ada@example.com")) match
      case Wf.Step.Asking(q) => assertEquals(q, "city")
      case other => fail(s"expected the third field, got $other")
    // an empty journal stands at the first
    run(typed)(Nil) match
      case Wf.Step.Asking(q) => assertEquals(q, "name")
      case other => fail(s"expected the first field, got $other")

  test("and the earlier answers are not asked again: the whole journal finishes it"):
    assertEquals(run(typed)(journalOf("Ada", "ada@example.com", "Kyiv")),
      Wf.Step.Done(Signup("Ada", "ada@example.com", "Kyiv")))
    assertEquals(run(derived)(journalOf("Ada", "ada@example.com", "Kyiv")),
      Wf.Step.Done(Vector("Ada", "ada@example.com", "Kyiv")))

  test("the position is a PATH, so a page can draw where the user stopped"):
    val at = Wf.Proc.walk(typed)((), journalOf("Ada")) match
      case Right(Wf.Proc.Standing.Asking(p, _, _)) => p
      case other => fail(s"expected a standing question, got $other")
    val m = typed.mermaid(Some(at))
    assertEquals("""class q\d+ here""".r.findAllIn(m).size, 1, m)

  /**
   * THE UI HALF. A standing question is a field to show, so the page
   * renders it with okay-ui's own machinery — a one-field `Ui.Form` —
   * and the answer comes back the way every other answer does, through
   * `Ui.submit`. Nothing here is special to `Proc`: a form is a form.
   */
  private def shown(q: String, value: String): Ui =
    Ui.Form(Vector(Ui.Input(value, key = q, label = q)), "next", q)

  test("a standing question renders as a form, and its submit is the answer"):
    val q = run(typed)(journalOf("Ada")) match
      case Wf.Step.Asking(q) => q
      case other => fail(s"expected a question, got $other")
    assertEquals(Frame.render(shown(q, "")).head, "email: []")
    val answer = Ui.submit(shown(q, "ada@example.com"), q) match
      case Some(Event.Submitted(_, edits)) =>
        edits.collectFirst { case Event.Edited(_, v) => v }.getOrElse("")
      case other => fail(s"expected a Submitted, got $other")
    assertEquals(answer, "ada@example.com")
    // appended to the journal, the run moves on
    run(typed)(journalOf("Ada", answer)) match
      case Wf.Step.Asking(next) => assertEquals(next, "city")
      case other => fail(s"expected the third field, got $other")
