package okay.persist

import munit.FunSuite
import okay.{!, +, Async, CanBlock, Delim, Pure, Wf}
import okay.given_CanBlock
import okay.Direct.*
import okay.codec.Schema
import scala.language.implicitConversions

/**
 * JVM ONLY, and the reason is the feature itself: these drive a
 * worker whose ACTIVITY ROW is `Async`, and running an Async row to a
 * value needs `CanBlock`, which Scala.js does not have and by design
 * never will (a browser cannot park). The model underneath is
 * platform-neutral and its tests — `TestTimers`, `TestWorkflow`,
 * `TestDialogue*` — stay cross-platform; on JS a worker is driven by
 * `Async.runAsync` into a Future instead.
 *
 * WHAT THE OUTSIDE WORLD SENDS A RUNNING WORKFLOW (workflow-signals,
 * 2026-09-17). An answer replies to a question the program asked; a
 * signal is sent whenever the sender has something to say — possibly
 * long before the run reaches the `awaitSignal` that wants it. The
 * mailbox is where the difference lives.
 */
class TestSignals extends FunSuite {

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_000L, id = "id", dice = 0.5)

  /**
   * The driver's row is NOT the program's (workflow-activity-row):
   * the workflow stays in the replayable `Pure`, while the ACTIVITIES
   * the oracle performs live in `Async` — which is the whole point,
   * and is why these tests run through `Async.run` rather than
   * `!.run`.
   */
  def drive[A](p: A ! Pure + Async)(using CanBlock): A =
    !.run(Async.run[A, Pure](p))


  def approval(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
    direct:
      val what = !w.pause("what?")
      val by = !w.awaitSignal("approved")
      s"$what by $by"

  def setup(store: MemoryStore) =
    val sigs = Signals.over(store)
    val w = Worker[String, String, String, Pure, Async](
      store.topic("approvals"), "approve/1", Timers.over(store),
      _ => okay.async("the budget"), signals = Some(sigs))(approval)
    (sigs, w)

  // ---- the mailbox, on its own

  test("the mailbox keeps order WITHIN a name, and names do not block each other") {
    val s = Signals.over(MemoryStore())
    val _ = s.send("d-1", "approved", "ada")
    val _ = s.send("d-1", "cancelled", "ops")
    val _ = s.send("d-1", "approved", "bob")

    val (off1, first) = s.next("d-1", "approved").get
    assertEquals(first, "ada")
    s.delivered("d-1", "approved", off1)
    assertEquals(s.next("d-1", "approved").map(_._2), Some("bob"))
    // the other name was never in the way, and is still waiting
    assertEquals(s.next("d-1", "cancelled").map(_._2), Some("ops"))
  }

  test("mail is per dialogue: one id's signals are not another's") {
    val s = Signals.over(MemoryStore())
    val _ = s.send("d-1", "approved", "ada")
    assertEquals(s.next("d-2", "approved"), None)
  }

  // ---- and through a worker

  test("a signal that arrives BEFORE the wait is delivered when the run gets there") {
    val store = MemoryStore()
    val (sigs, w) = setup(store)

    // the approver is quick, the run has not even started
    val _ = sigs.send("a-1", "approved", "ada")

    assertEquals(drive(w.start("a-1")), Worker.Progress.Finished("the budget by ada"))
  }

  test("a run that waits first is woken when the signal arrives") {
    val store = MemoryStore()
    val (sigs, w) = setup(store)

    assertEquals(drive(w.start("a-1")), Worker.Progress.Waiting(Wf.Wait.Signal("approved")))
    val _ = sigs.send("a-1", "approved", "bob")
    assertEquals(drive(w.advance("a-1")), Worker.Progress.Finished("the budget by bob"))
  }

  test("the cursor moves only after the journal took it, so the signal is not re-eaten") {
    val store = MemoryStore()
    val (sigs, w) = setup(store)
    val _ = sigs.send("a-1", "approved", "ada")
    val _ = sigs.send("a-1", "approved", "bob")   // a second, for a later run
    val _ = drive(w.start("a-1"))

    // exactly one was consumed, and it is the first
    assertEquals(sigs.next("a-1", "approved").map(_._2), Some("bob"))
    assertEquals(w.dialogue("a-1").journal.collect { case Left(Wf.SysA.Got(v)) => v },
      List("ada"))
  }

  test("a signal for a name nobody waits on stays in the box") {
    val store = MemoryStore()
    val (sigs, w) = setup(store)
    val _ = sigs.send("a-1", "cancelled", "ops")

    assertEquals(drive(w.start("a-1")), Worker.Progress.Waiting(Wf.Wait.Signal("approved")))
    // the letter is untouched: it was never for this wait
    assertEquals(sigs.next("a-1", "cancelled").map(_._2), Some("ops"))
    assertEquals(sigs.cursor("a-1", "cancelled"), None)
  }

  test("a worker with NO mailbox simply reports the wait") {
    val store = MemoryStore()
    val bare = Worker[String, String, String, Pure, Async](
      store.topic("approvals"), "approve/1", Timers.over(store),
      _ => okay.async("the budget"))(approval)
    assertEquals(drive(bare.start("a-1")), Worker.Progress.Waiting(Wf.Wait.Signal("approved")))
  }
}
