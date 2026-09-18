import okay.*
import okay.Proc.given
import scala.language.implicitConversions

/**
 * THE DEPLOY CHECK (specs/static-workflow.md stage 2): which live runs
 * would this term strand, asked BEFORE the deploy and without starting
 * one of them.
 *
 * Failure B of specs/durable-workflow.md is a changed program reading
 * an old journal — silently at first, and a loud stop since the
 * envelope carried a `program` field. A loud stop is right and it is
 * still an OUTAGE: it happens in production, to a run, at the moment
 * somebody deploys. A term moves the same question to before the
 * deploy, because `walk` performs nothing.
 */
class TestProcStrands extends munit.FunSuite:

  type P = okay.Pure
  type Row = Delim + P
  type Sig = Wf.Asked[String, String]

  def ask(q: String): Wf.Question[String, String, String] = Wf.Question.Ask(q)
  def patch(id: String): Wf.Question[String, String, Boolean] = Wf.Question.Patched(id)

  given Wf.Runtime = Wf.Runtime.scripted(millis = 7L, id = "id-1", dice = 0.25)

  /** v1: the program the live runs were started under */
  val v1: Wf.Proc[String, String, Unit, String] =
    Proc.direct: _ =>
      val city: String = ask("city?")
      val n: String = ask("nights?")
      s"$city/$n"

  /** v2: a question added BEFORE everything — the shape that strands */
  val v2: Wf.Proc[String, String, Unit, String] =
    Proc.direct: _ =>
      val who: String = ask("who?")
      val city: String = ask("city?")
      val n: String = ask("nights?")
      s"$who/$city/$n"

  /** v3: the same change made the way that DOES NOT strand — a patch,
   * which a journal written before it answers `false` without being
   * consumed */
  val v3: Wf.Proc[String, String, Unit, String] =
    Proc.direct: _ =>
      val city: String = ask("city?")
      val extra: String = if patch("greeting") then ask("who?") else ""
      val n: String = ask("nights?")
      s"$extra/$city/$n"

  val started: Wf.Journal[String] = List(Right("Kyiv"))
  val halfway: Wf.Journal[String] = List(Right("Kyiv"), Right("3"))

  test("WHAT THE CHECK CANNOT SEE, and it is the first thing to know"):
    // A journal holds ANSWERS. Two author questions whose answers have
    // the same type are indistinguishable in it — so a term that adds
    // a question BEFORE them reads every old answer one place across
    // and carries on, which is failure B of specs/durable-workflow.md
    // exactly. `walk` accepts it, and pretending otherwise would be
    // worse than saying so:
    Wf.Proc.walk(v2)((), halfway) match
      case Right(Wf.Proc.Standing.Asking(_, q, accepted)) =>
        assertEquals(Wf.Proc.tag(q), Right("nights?"))
        assertEquals(accepted, 2,
          "the old answers were read onto the new questions, one place across")
      case other => fail(s"expected the silent mis-mapping this test documents, got $other")
    // and `strands` therefore says nothing about it
    assertEquals(Wf.Proc.strands(v2)(())(List("run-b" -> halfway)), Map.empty)

  test("what it DOES see: an answer whose SHAPE the new term cannot take"):
    // a question the RUNTIME answers has a tagged answer, so a term
    // that puts one where an author's answer sits is caught at the
    // record, with no run started
    val stamped: Wf.Proc[String, String, Unit, String] =
      Proc.direct: _ =>
        val t: Long = Wf.Question.Now()
        val city: String = ask("city?")
        s"$city@$t"
    Wf.Proc.walk(stamped)((), halfway) match
      case Left(bad) =>
        assertEquals(bad.record, 0, "the stranding is at the very first record")
        assert(bad.why.contains("Now"), bad.why)
      case other => fail(s"a v1 journal was accepted where a clock reading belongs: $other")

  test("v1's own journals are accepted — the check does not cry wolf"):
    assert(Wf.Proc.accepts(v1)((), started))
    assert(Wf.Proc.accepts(v1)((), halfway))

  test("strands names every run the term would strand, and no other"):
    val live = List(
      "run-a" -> started,
      "run-b" -> halfway,
      "run-c" -> (Nil: Wf.Journal[String]))
    val stamped: Wf.Proc[String, String, Unit, String] =
      Proc.direct: _ =>
        val t: Long = Wf.Question.Now()
        val city: String = ask("city?")
        s"$city@$t"
    val bad = Wf.Proc.strands(stamped)(())(live)
    assertEquals(bad.keySet, Set("run-a", "run-b"),
      "a run with an empty journal has nothing to strand on")
    assertEquals(bad("run-a").record, 0)
    // and the term that was changed with a PATCH strands nobody
    assertEquals(Wf.Proc.strands(v3)(())(live), Map.empty)

  test("the patched term carries the old runs AND takes the new branch for fresh ones"):
    // the old run: the patch answers false without eating `3`
    assertEquals(Wf.Proc.walk(v3)((), halfway),
      Right(Wf.Proc.Standing.Done("/Kyiv/3")))
    // a fresh run stands AT the patch after its first answer
    Wf.Proc.walk(v3)((), started) match
      case Right(Wf.Proc.Standing.Asking(_, q, _)) =>
        assertEquals(Wf.Proc.tag(q), Left(Wf.Sys.Patch("greeting")))
      case other => fail(s"expected to stand at the patch, got $other")

  test("the check performs NOTHING — its signature is the proof"):
    // no Runtime, no row, no monad: `strands` is a pure function of a
    // term and some journals, which is why it can be run against ten
    // thousand of them before a deploy rather than during one
    val stamped: Wf.Proc[String, String, Unit, String] =
      Proc.direct: _ =>
        val t: Long = Wf.Question.Now()
        val city: String = ask("city?")
        s"$city@$t"
    val many = (1 to 500).map(i => s"run-$i" -> halfway).toList
    assertEquals(Wf.Proc.strands(stamped)(())(many).size, 500)
    assertEquals(Wf.Proc.strands(v1)(())(many), Map.empty)
