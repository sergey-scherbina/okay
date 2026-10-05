package okay2.persist

import munit.FunSuite
import okay2.{!, pure}
import okay2.workflow.Wf
import okay2.optics.Optic.arrows._

/**
 * THE EXHAUSTIVE CUT (okay-persist's TestProcCut; specs/static-workflow.md
 * stage 2): a term has finitely many leaves, so "a crash resumes
 * correctly" can be a PROPERTY — crash at every one, resume, compare.
 * `Dialogue` advances and THEN appends, so an oracle that throws on its
 * k-th call leaves k-1 answers durable and the k-th activity performed
 * but unrecorded: the at-least-once window, measured.
 */
class TestProcCut extends FunSuite {
  import DialogueFixtures._
  import WorkflowFixtures._
  import ProcFixtures._

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 7L, id = "id-1", dice = 0.25)

  /** four activities, one after another — every one an outside call */
  val booking: Wf.Proc[String, String, Unit, String] =
    Wf.Proc.ask[String, String, Unit](_ => "city?") >>>
      keep(Wf.Proc.ask[String, String, String](city => s"hotel in $city?")) >>>
      keep(Wf.Proc.ask[String, String, (String, String)](_ => "nights?")) >>>
      keep(Wf.Proc.ask[String, String, ((String, String), String)](_ => "card?")) >>>
      A.arr((p: (((String, String), String), String)) => s"${p._1._1._1}/${p._1._1._2}/${p._1._2}/${p._2}")

  val leaves: Int = booking.leaves.length

  val answers: Map[String, String] = Map("city?" -> "Kyiv", "hotel in Kyiv?" -> "Opera", "nights?" -> "3", "card?" -> "4242")

  class Boom extends RuntimeException("the process died")

  def wfOf(t: Topic, id: String): Dialogue[Wf.Ask[String], Wf.Ans[String], String, P] = wf(t, id, "booking/1")(term(booking))

  /** drive, counting every activity, and die on the `cutAt`-th call
   * AFTER it has run — the card was charged, the answer never reached
   * the log */
  def drive(t: Topic, id: String, calls: scala.collection.mutable.Map[String, Int], cutAt: Int): Either[Wf.Wait, String] = {
    var n = 0
    !.run(wfOf(t, id).runWorkflow { (q, _) =>
      n += 1
      calls(q) = calls.getOrElse(q, 0) + 1
      if (n == cutAt) throw new Boom
      pure[P, String](answers(q))
    })
  }

  test("the uninterrupted run is the control") {
    val t = new MemoryStore().topic("cut-control")
    val calls = scala.collection.mutable.Map.empty[String, Int]
    assertEquals(drive(t, "c", calls, cutAt = 0), Right("Kyiv/Opera/3/4242"))
    assertEquals(calls.toMap, answers.keys.map(_ -> 1).toMap)
    assertEquals(leaves, 4)
  }

  test("a crash at EVERY leaf: the answer is the same, and the window is one call") {
    for (cut <- 1 to leaves) {
      val t = new MemoryStore().topic(s"cut-$cut")
      val calls = scala.collection.mutable.Map.empty[String, Int]

      // the first process dies on its `cut`-th question
      intercept[Boom](drive(t, "r", calls, cutAt = cut))
      assertEquals(wfOf(t, "r").recovered.answers.length, cut - 1,
        s"cut $cut: the answers before the crash are durable and the one in flight is not")

      // a new process picks the journal up and finishes
      assertEquals(drive(t, "r", calls, cutAt = 0), Right("Kyiv/Opera/3/4242"), s"cut $cut answered differently")

      // every activity ran once except the one the crash caught in
      // flight, which ran twice
      val twice = calls.filter(_._2 != 1)
      assertEquals(twice.size, 1, s"cut $cut: $calls")
      assertEquals(twice.head._2, 2, s"cut $cut: $calls")
      assertEquals(calls.values.sum, leaves + 1, s"cut $cut: $calls")
    }
  }

  test("the LAST leaf is the worst case, and it is still one call") {
    val t = new MemoryStore().topic("cut-last")
    val calls = scala.collection.mutable.Map.empty[String, Int]
    intercept[Boom](drive(t, "l", calls, cutAt = leaves))
    assertEquals(wfOf(t, "l").recovered.answers.length, leaves - 1)
    assertEquals(drive(t, "l", calls, cutAt = 0), Right("Kyiv/Opera/3/4242"))
    assertEquals(calls("card?"), 2, "the activity the crash caught in flight")
    assertEquals(calls.values.sum, leaves + 1)
  }

  test("the TERM agrees about where each cut left the run, with no runtime") {
    for (cut <- 1 to leaves) {
      val t = new MemoryStore().topic(s"cut-walk-$cut")
      val calls = scala.collection.mutable.Map.empty[String, Int]
      intercept[Boom](drive(t, "w", calls, cutAt = cut))
      val j = wfOf(t, "w").recovered.answers
      Wf.Proc.walk(booking)((), j) match {
        case Right(Wf.Proc.Standing.Asking(_, q, accepted)) =>
          assertEquals(accepted, cut - 1)
          assert(Wf.Proc.tag(q).isRight, s"cut $cut stands on a library question")
        case other => fail(s"cut $cut: expected a standing question, got $other")
      }
    }
  }
}
