package okay2.persist

import munit.FunSuite
import okay2.{!}
import okay2.workflow.Wf
import okay2.async.Async

/**
 * ASKING A RUN TO STOP (okay-persist's TestCancel; workflow-cancel): a
 * decision a run has ALREADY MADE is replayed, not re-decided —
 * withdraw the request after the fact and the run still finishes the
 * way it finished, because it decided from its journal.
 */
class TestCancel extends FunSuite {
  import DialogueFixtures._
  import WorkflowFixtures._
  import WorkerFixtures._

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1000L, id = "id", dice = 0.5)

  /** checks twice: once before the nap, once after */
  def job(w: W): String ! Rw = for {
    who <- w.pause("who?")
    early <- w.cancelled
    _ <- w.sleep(60000L)
    late <- w.cancelled
  } yield s"$who:${early.getOrElse("-")}:${late.getOrElse("-")}"

  def worker(store: MemoryStore, cancels: Option[Cancels]): Worker[String, String, String, P, Async] =
    new Worker[String, String, String, P, Async](store.topic("jobs"), "job/1", Timers.over(store), say("ada"), cancels = cancels)(job)

  def finished(s: String): List[(String, Worker.Progress[String])] = List("j-1" -> Worker.Progress.Finished(s))

  test("nobody asked: both checks say no") {
    val store = new MemoryStore
    val w = worker(store, Some(Cancels.over(store)))
    assertEquals(drive(w.start("j-1")), Worker.Progress.Sleeping(61000L): Worker.Progress[String])
    assertEquals(drive(w.tick(61000L)), finished("ada:-:-"))
  }

  test("a request that arrives mid-run is seen at the NEXT check, not the last one") {
    val store = new MemoryStore
    val w = worker(store, Some(Cancels.over(store)))
    val _ = drive(w.start("j-1"))        // the early check is already journalled: no

    assert(w.cancel("j-1", "budget"), "the worker had nowhere to put the request")
    assertEquals(drive(w.tick(61000L)), finished("ada:-:budget"))
  }

  test("a worker built without a cancel topic REFUSES rather than dropping the request") {
    val store = new MemoryStore
    val w = worker(store, None)
    assertEquals(w.cancel("j-1", "budget"), false)
    val _ = drive(w.start("j-1"))
    assertEquals(drive(w.tick(61000L)), finished("ada:-:-"))
  }

  test("withdrawing BEFORE the check is seen; the run is not cancelled") {
    val store = new MemoryStore
    val cancels = Cancels.over(store)
    val w = worker(store, Some(cancels))
    val _ = drive(w.start("j-1"))
    cancels.cancel("j-1", "budget")
    cancels.withdraw("j-1")
    assertEquals(cancels.requested("j-1"), None)
    assertEquals(drive(w.tick(61000L)), finished("ada:-:-"))
  }

  test("THE POINT: a decision already made is replayed, not re-decided") {
    val store = new MemoryStore
    val cancels = Cancels.over(store)
    val w = worker(store, Some(cancels))
    val _ = drive(w.start("j-1"))
    cancels.cancel("j-1", "budget")
    assertEquals(drive(w.tick(61000L)), finished("ada:-:budget"))

    cancels.withdraw("j-1")
    assertEquals(drive(w.advance("j-1")), Worker.Progress.Finished("ada:-:budget"): Worker.Progress[String],
      "the run re-decided from the cancel topic instead of from its journal")

    val answers = w.dialogue("j-1").journal.collect { case Left(a) => a }
    assert(answers.contains(Wf.SysA.Flag(false)), s"the first check is not recorded: $answers")
    assert(answers.contains(Wf.SysA.Text("budget")), s"the second check is not recorded: $answers")
  }

  test("the topic itself: newest reason wins, tombstone clears") {
    val c = Cancels.over(new MemoryStore)
    assertEquals(c.requested("x"), None)
    c.cancel("x", "first")
    c.cancel("x", "second")
    assertEquals(c.requested("x"), Some("second"))
    assertEquals(c.requests, Map("x" -> "second"))
    c.withdraw("x")
    assertEquals(c.requests, Map.empty[String, String])
  }
}
