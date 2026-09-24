package okay.persist

import munit.FunSuite
import okay.{!, +, Async, CanBlock, Delim, Pure, Wf}
import okay.given_CanBlock
import okay.given_Scheduler
import okay.given_Timer
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
 * THE LOOP THAT CARRIES WORKFLOWS FORWARD (workflow-worker,
 * 2026-09-17). The interesting tests are the last three: a stale
 * timer must cost a read and nothing else, two workers must produce
 * ONE journal, and a worker that dies must leave the next one able to
 * carry on.
 */
class TestWorker extends FunSuite {

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


  /** answer, sleep a minute, finish */
  def nap(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure = direct:
    val who = !w.pause("who?")
    !w.sleep(60_000L)
    s"$who woke"

  def worker(store: MemoryStore, t: Topic, answers: String = "ada") =
    Worker[String, String, String, Pure, Async](t, "nap/1", Timers.over(store),
      _ => okay.async(answers))(nap)

  test("start drives a run to its sleep and ARMS the deadline") {
    val store = MemoryStore()
    val t = store.topic("naps")
    val w = worker(store, t)

    assertEquals(drive(w.start("n-1")), Worker.Progress.Sleeping(1_000L + 60_000L))
    assertEquals(Timers.over(store).armed, Map("n-1" -> 61_000L))
  }

  test("tick before the deadline does nothing; after it, the run finishes") {
    val store = MemoryStore()
    val t = store.topic("naps")
    val w = worker(store, t)
    val _ = drive(w.start("n-1"))

    assertEquals(drive(w.tick(60_000L)), Nil, "a deadline that has not passed fired")
    assertEquals(drive(w.tick(61_000L)), List("n-1" -> Worker.Progress.Finished("ada woke")))
    // and a finished run is disarmed, so it is never picked up again
    assertEquals(Timers.over(store).armed, Map.empty[String, Long])
    assertEquals(drive(w.tick(999_999L)), Nil)
  }

  test("a STALE timer costs a read and nothing else") {
    val store = MemoryStore()
    val t = store.topic("naps")
    val w = worker(store, t)
    val _ = drive(w.start("n-1"))
    val _ = drive(w.tick(61_000L))            // finished and disarmed
    val journal = w.dialogue("n-1").journal

    // somebody re-arms a run that has moved on — a duplicate delivery,
    // a replayed operational record, an operator's mistake
    Timers.over(store).arm("n-1", 1L)
    val progress = drive(w.tick(999_999L))
    assertEquals(progress.map(_._2), List(Worker.Progress.Finished("ada woke")))

    // THE POINT: the journal did not gain an answer nobody asked for
    assertEquals(w.dialogue("n-1").journal, journal)
  }

  test("two workers on one dialogue produce ONE journal") {
    val store = MemoryStore()
    val t = store.topic("naps")
    val a = worker(store, t, "ada")
    val b = worker(store, t, "bob")

    // both start the same id from the same standing start
    val ra = drive(a.start("n-1"))
    val rb = drive(b.start("n-1"))
    assertEquals(ra, Worker.Progress.Sleeping(61_000L))
    // the second one found the first one's answer already in the log
    assertEquals(rb, Worker.Progress.Sleeping(61_000L))

    val j = a.dialogue("n-1").journal
    assertEquals(j.count(_.isRight), 1, s"the question was answered twice: $j")
    assertEquals(j.head, Right("ada"))
  }

  test("a worker that dies leaves the next one able to carry on") {
    val store = MemoryStore()
    val t = store.topic("naps")
    val first = worker(store, t)
    val _ = drive(first.start("n-1"))

    // ---- this worker dies here, holding nothing

    val second = worker(store, t, answers = "never asked")
    assertEquals(drive(second.tick(61_000L)),
      List("n-1" -> Worker.Progress.Finished("ada woke")),
      "the second worker re-asked the question instead of reading the log")
  }

  test("a run waiting on a SIGNAL is disarmed: the clock is not what wakes it") {
    val store = MemoryStore()
    val t = store.topic("signals")
    def approve(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
      direct:
        val what = !w.pause("what?")
        val by = !w.awaitSignal("approval")
        s"$what by $by"
    val w = Worker[String, String, String, Pure, Async](t, "approve/1", Timers.over(store),
      _ => okay.async("the budget"))(approve)

    assertEquals(drive(w.start("a-1")), Worker.Progress.Waiting(Wf.Wait.Signal("approval")))
    assertEquals(Timers.over(store).armed, Map.empty[String, Long],
      "a signal-waiter was left on the clock's list")
  }

  // ==== the two rows, and why there are two =========================

  test("an ACTIVITY may reach outside; the PROGRAM may not") {
    val store = MemoryStore()
    val t = store.topic("io")
    var performed = 0

    // the oracle's work lives in the activity row: typed, not smuggled
    // past the effect system in a closure
    val w = Worker[String, String, String, Pure, Async](
      t, "nap/1", Timers.over(store),
      _ => okay.async { performed += 1; "ada" })(nap)

    assertEquals(drive(w.start("n-1")), Worker.Progress.Sleeping(61_000L))
    assertEquals(performed, 1)

    // and the PROGRAM's row still refuses it, which is the half that
    // makes replay exact: a durable program that could call a service
    // between two pauses would call it again on every replay
    val e = compileErrors("""
      okay.persist.Dialogue.workflow[String, String, String, okay.Async](
        okay.persist.MemoryStore().topic("t"), "d", "p/1")(
          okay.Direct.direct(""))""")
    assert(e.nonEmpty, "an Async-rowed workflow compiled")
    assert(e.contains("PERFORM AGAIN"), s"refused for the wrong reason: $e")
  }

  // ==== retries: the driver's business, not the program's ==========

  test("an activity that fails twice is retried, and the journal gains ONE answer") {
    val store = MemoryStore()
    val t = store.topic("retried")
    var attempts = 0
    val flaky: String => String ! Async = _ => okay.async:
      attempts += 1
      if attempts < 3 then throw new RuntimeException(s"attempt $attempts failed")
      "ada"

    val w = Worker[String, String, String, Pure, Async](
      t, "nap/1", Timers.over(store),
      Worker.retrying(okay.Retry.immediate(5))(q => flaky(q)))(nap)

    assertEquals(drive(w.start("n-1")), Worker.Progress.Sleeping(61_000L))
    assertEquals(attempts, 3, "the driver did not retry")
    // THE POINT: the program asked once and was answered once
    assertEquals(w.dialogue("n-1").journal.count(_.isRight), 1)
    assertEquals(w.dialogue("n-1").journal.head, Right("ada"))
  }

  test("an exhausted policy gives up FOR NOW: nothing is journalled and the run stands") {
    val store = MemoryStore()
    val t = store.topic("exhausted")
    var attempts = 0
    val broken: String => String ! Async = _ => okay.async:
      attempts += 1
      throw new RuntimeException("the service is down")

    val w = Worker[String, String, String, Pure, Async](
      t, "nap/1", Timers.over(store),
      Worker.retrying(okay.Retry.immediate(2))(q => broken(q)))(nap)

    val _ = intercept[RuntimeException](drive(w.start("n-1")))
    assertEquals(attempts, 3, "one attempt plus two retries")
    // nothing was written, so the run is exactly where it began
    assertEquals(w.dialogue("n-1").journal, Nil)

    // ---- the service comes back; a later worker finishes the run
    val ok = Worker[String, String, String, Pure, Async](
      t, "nap/1", Timers.over(store), _ => okay.async("bob"))(nap)
    assertEquals(drive(ok.start("n-1")), Worker.Progress.Sleeping(61_000L))
    assertEquals(ok.dialogue("n-1").journal.head, Right("bob"))
  }
}
