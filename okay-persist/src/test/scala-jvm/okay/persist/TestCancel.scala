package okay.persist

import munit.FunSuite
import okay.{!, +, Async, CanBlock, Delim, Pure, Wf}
import okay.given_CanBlock
import okay.Direct.*
import okay.codec.Schema
import scala.language.implicitConversions

/**
 * ASKING A RUN TO STOP (workflow-cancel, 2026-09-17).
 *
 * The fifth test is the one that matters, and it is why cancellation
 * is shaped this way: a decision a run has ALREADY MADE is replayed,
 * not re-decided. Withdraw the request after the fact and the run
 * still finishes the way it finished — because it decided from its
 * journal, not from the cancel topic.
 *
 * JVM only for the same reason as `TestWorker`: the activity row is
 * `Async`, and running one to a value needs `CanBlock`.
 */
class TestCancel extends FunSuite {

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_000L, id = "id", dice = 0.5)

  def drive[A](p: A ! Pure + Async)(using CanBlock): A =
    !.run(Async.run[A, Pure](p))

  /** checks twice: once before the nap, once after */
  def job(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
    direct:
      val who = !w.pause("who?")
      val early = !w.cancelled
      !w.sleep(60_000L)
      val late = !w.cancelled
      s"$who:${early.getOrElse("-")}:${late.getOrElse("-")}"

  def worker(store: MemoryStore, cancels: Option[Cancels]) =
    Worker[String, String, String, Pure, Async](store.topic("jobs"), "job/1",
      Timers.over(store), _ => okay.async("ada"), cancels = cancels)(job)

  test("nobody asked: both checks say no") {
    val store = MemoryStore()
    val w = worker(store, Some(Cancels.over(store)))
    assertEquals(drive(w.start("j-1")), Worker.Progress.Sleeping(61_000L))
    assertEquals(drive(w.tick(61_000L)),
      List("j-1" -> Worker.Progress.Finished("ada:-:-")))
  }

  test("a request that arrives mid-run is seen at the NEXT check, not the last one") {
    val store = MemoryStore()
    val w = worker(store, Some(Cancels.over(store)))
    val _ = drive(w.start("j-1"))        // the early check is already journalled: no

    assert(w.cancel("j-1", "budget"), "the worker had nowhere to put the request")
    assertEquals(drive(w.tick(61_000L)),
      List("j-1" -> Worker.Progress.Finished("ada:-:budget")))
  }

  test("a worker built without a cancel topic REFUSES rather than dropping the request") {
    val store = MemoryStore()
    val w = worker(store, None)
    assertEquals(w.cancel("j-1", "budget"), false)
    val _ = drive(w.start("j-1"))
    assertEquals(drive(w.tick(61_000L)),
      List("j-1" -> Worker.Progress.Finished("ada:-:-")))
  }

  test("withdrawing BEFORE the check is seen; the run is not cancelled") {
    val store = MemoryStore()
    val cancels = Cancels.over(store)
    val w = worker(store, Some(cancels))
    val _ = drive(w.start("j-1"))
    cancels.cancel("j-1", "budget")
    cancels.withdraw("j-1")
    assertEquals(cancels.requested("j-1"), None)
    assertEquals(drive(w.tick(61_000L)),
      List("j-1" -> Worker.Progress.Finished("ada:-:-")))
  }

  test("THE POINT: a decision already made is replayed, not re-decided") {
    val store = MemoryStore()
    val cancels = Cancels.over(store)
    val w = worker(store, Some(cancels))
    val _ = drive(w.start("j-1"))
    cancels.cancel("j-1", "budget")
    assertEquals(drive(w.tick(61_000L)),
      List("j-1" -> Worker.Progress.Finished("ada:-:budget")))

    // the request is gone — and the run's history does not change with it
    cancels.withdraw("j-1")
    assertEquals(drive(w.advance("j-1")), Worker.Progress.Finished("ada:-:budget"),
      "the run re-decided from the cancel topic instead of from its journal")

    // and the journal says so: two Sys answers, no and then why
    val answers = w.dialogue("j-1").journal.collect { case Left(a) => a }
    assert(answers.contains(Wf.SysA.Flag(false)), s"the first check is not recorded: $answers")
    assert(answers.contains(Wf.SysA.Text("budget")), s"the second check is not recorded: $answers")
  }

  test("the topic itself: newest reason wins, tombstone clears") {
    val store = MemoryStore()
    val c = Cancels.over(store)
    assertEquals(c.requested("x"), None)
    c.cancel("x", "first")
    c.cancel("x", "second")
    assertEquals(c.requested("x"), Some("second"))
    assertEquals(c.requests, Map("x" -> "second"))
    c.withdraw("x")
    assertEquals(c.requests, Map.empty[String, String])
  }
}
