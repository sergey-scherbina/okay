package okay.persist

import munit.FunSuite
import okay.{!, Proc, Pure, Wf}
import okay.Proc.given
import okay.codec.Schema
import scala.language.implicitConversions

/**
 * THE EXHAUSTIVE CUT (specs/static-workflow.md stage 2).
 *
 * "A crash resumes correctly" is normally a SAMPLE: somebody picks a
 * plausible moment, kills the run and checks the answer. A term has
 * finitely many leaves, so the same claim can be a PROPERTY — crash at
 * every one of them, resume, and compare — and that is most of why the
 * static shape exists.
 *
 * WHERE THE CUT FALLS, exactly. `Dialogue` advances the program and
 * THEN appends (specs/durable-workflow.md stage 0: a trial before a
 * commit, so an answer the program refuses is not journalled forever).
 * An oracle that throws on its k-th call therefore leaves k-1 answers
 * durable and the k-th activity performed but unrecorded — which is
 * the at-least-once window every workflow engine has, and the thing
 * this test measures rather than assumes.
 */
class TestProcCut extends FunSuite {

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 7L, id = "id-1", dice = 0.25)

  type Sig = Wf.Asked[String, String]

  def ask(q: String): Wf.Question[String, String, String] = Wf.Question.Ask(q)

  /** four activities, one after another — every one an outside call */
  val booking: Wf.Proc[String, String, Unit, String] =
    Proc.direct: _ =>
      val city: String = ask("city?")
      val hotel: String = ask(s"hotel in $city?")
      val nights: String = ask("nights?")
      val card: String = ask("card?")
      s"$city/$hotel/$nights/$card"

  val leaves: Int = booking.leaves.length

  val answers: Map[String, String] = Map(
    "city?" -> "Kyiv", "hotel in Kyiv?" -> "Opera",
    "nights?" -> "3", "card?" -> "4242")

  class Boom extends RuntimeException("the process died")

  def wf(t: Topic, id: String) =
    Dialogue.workflow[String, String, String, Pure](t, id, "booking/1")(
      Wf.Proc.program(booking)(()))

  /**
   * Drive, counting every activity, and die on the `cutAt`-th call
   * AFTER it has run.
   *
   * The order inside this lambda is the whole fidelity of the test,
   * and the first cut had it the other way: throwing BEFORE the count
   * models a process that died without making its outside call, which
   * is not the window anybody worries about. The window is that the
   * call HAPPENED — the card was charged — and the answer never
   * reached the log.
   */
  def drive(t: Topic, id: String, calls: scala.collection.mutable.Map[String, Int],
            cutAt: Int): Either[Wf.Wait, String] =
    var n = 0
    !.run(wf(t, id).runWorkflow { q =>
      n += 1
      calls(q) = calls.getOrElse(q, 0) + 1
      if n == cutAt then throw new Boom
      okay.pure(answers(q))
    })

  test("the uninterrupted run is the control") {
    val t = MemoryStore().topic("cut-control")
    val calls = scala.collection.mutable.Map.empty[String, Int]
    assertEquals(drive(t, "c", calls, cutAt = 0), Right("Kyiv/Opera/3/4242"))
    assertEquals(calls.toMap, answers.keys.map(_ -> 1).toMap)
    assertEquals(leaves, 4)
  }

  test("a crash at EVERY leaf: the answer is the same, and the window is one call") {
    for cut <- 1 to leaves do
      val t = MemoryStore().topic(s"cut-$cut")
      val calls = scala.collection.mutable.Map.empty[String, Int]

      // the first process dies on its `cut`-th question
      val _ = intercept[Boom](drive(t, "r", calls, cutAt = cut))
      val durable = wf(t, "r").recovered.answers.length
      assertEquals(durable, cut - 1,
        s"cut $cut: the answers before the crash are durable and the one in flight is not")

      // a new process picks the journal up and finishes
      val out = drive(t, "r", calls, cutAt = 0)
      assertEquals(out, Right("Kyiv/Opera/3/4242"), s"cut $cut answered differently")

      // AND THE FLOOR, MEASURED: every activity ran once except the one
      // the crash caught in flight, which ran twice — at-least-once,
      // with a window of exactly one call
      val twice = calls.filter(_._2 != 1)
      assertEquals(twice.size, 1, s"cut $cut: $calls")
      assertEquals(twice.head._2, 2, s"cut $cut: $calls")
      assertEquals(calls.values.sum, leaves + 1, s"cut $cut: $calls")
  }

  test("the LAST leaf is the worst case, and it is still one call") {
    // a crash on the final activity: its answer never reaches the log,
    // so a new process asks it again and the run finishes. This is the
    // cut an engine is usually asked about, and it is not special —
    // the loop above covers it, this names it.
    val t = MemoryStore().topic("cut-last")
    val calls = scala.collection.mutable.Map.empty[String, Int]
    val _ = intercept[Boom](drive(t, "l", calls, cutAt = leaves))
    assertEquals(wf(t, "l").recovered.answers.length, leaves - 1)
    assertEquals(drive(t, "l", calls, cutAt = 0), Right("Kyiv/Opera/3/4242"))
    assertEquals(calls("card?"), 2, "the activity the crash caught in flight")
    assertEquals(calls.values.sum, leaves + 1)
  }

  test("the TERM agrees about where each cut left the run, with no runtime") {
    // the same property read the other way: after a crash at cut k the
    // journal has k-1 answers, and `walk` — which performs nothing —
    // says the run stands at the k-th question
    for cut <- 1 to leaves do
      val t = MemoryStore().topic(s"cut-walk-$cut")
      val calls = scala.collection.mutable.Map.empty[String, Int]
      val _ = intercept[Boom](drive(t, "w", calls, cutAt = cut))
      val j = wf(t, "w").recovered.answers
      Wf.Proc.walk(booking)((), j) match
        case Right(Wf.Proc.Standing.Asking(_, q, accepted)) =>
          assertEquals(accepted, cut - 1)
          assert(Wf.Proc.tag(q).isRight, s"cut $cut stands on a library question")
        case other => fail(s"cut $cut: expected a standing question, got $other")
  }
}
