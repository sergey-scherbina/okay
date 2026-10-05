package okay2.persist

import munit.FunSuite
import okay2.{!}
import okay2.workflow.Wf
import okay2.async.Async

/**
 * BOUNDED HISTORY, END TO END (okay-persist's TestContinueAsWorker;
 * dialogue-continue-as): a run that has been through four chapters has
 * a journal of ONE answer, so its cold start replays one answer rather
 * than everything that ever happened to it.
 */
class TestContinueAsWorker extends FunSuite {
  import DialogueFixtures._
  import WorkflowFixtures._
  import WorkerFixtures._

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1000L, id = "id", dice = 0.5)

  type Out = Wf.Next[String, String]
  type WO = Wf.Asks[String, String, Out, P]

  /** grows its input a letter at a time, one chapter per letter; the
   * FIRST pause is the input — the oracle's for a fresh run, the seed
   * for a continued one */
  def stage(w: WO): Out ! Rw = w.pause("input").map { input =>
    if (input.length >= 4) Wf.Next.Done(s"done:$input") else Wf.Next.Continue(input + "x")
  }

  val seed: Out => Option[String] = n => Wf.Next.seed(n)

  test("THE POINT: four chapters, a journal of one answer") {
    val store = new MemoryStore
    var asked = 0
    val w = new Worker[String, String, Out, P, Async](store.topic("stages"), "stage/1", Timers.over(store),
      say { asked += 1; "a" }, seedOf = seed)(stage)

    assertEquals(drive(w.start("s-1")), Worker.Progress.Finished(Wf.Next.Done("done:axxx")): Worker.Progress[Out])

    val d = w.dialogue("s-1")
    assertEquals(d.journal.size, 1, s"the history was not bounded: ${d.journal}")
    assertEquals(d.journal, List(Right("axxx")))
    assertEquals(d.recovered.accepted, 4, "the record count should span every chapter")

    // and the outside world was asked once, for the first chapter
    assertEquals(asked, 1)
  }

  test("a run that only ever continues hands back instead of spinning") {
    val store = new MemoryStore
    def forever(w: WO): Out ! Rw = w.pause("input").map(input => Wf.Next.Continue(input + "x"))

    val w = new Worker[String, String, Out, P, Async](store.topic("spin"), "forever/1", Timers.over(store),
      say("a"), seedOf = seed, continuations = 3)(forever)

    assertEquals(drive(w.start("s-1")), Worker.Progress.Continued(3): Worker.Progress[Out])
    assertEquals(drive(w.advance("s-1")), Worker.Progress.Continued(3): Worker.Progress[Out])
    assertEquals(w.dialogue("s-1").journal.size, 1)
  }

  test("a workflow that never continues is untouched by any of this") {
    val store = new MemoryStore
    def plain(w: W): String ! Rw = w.pause("who?").map(who => s"$who woke")
    val w = new Worker[String, String, String, P, Async](store.topic("plain"), "plain/1", Timers.over(store), say("ada"))(plain)

    assertEquals(drive(w.start("p-1")), Worker.Progress.Finished("ada woke"): Worker.Progress[String])
    assertEquals(w.dialogue("p-1").journal, List(Right("ada")))
  }
}
