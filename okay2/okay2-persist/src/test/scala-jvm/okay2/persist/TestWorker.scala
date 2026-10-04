package okay2.persist

import munit.FunSuite
import okay2.{!, Wf}
import okay2.async.{Async, Retry}
import okay2.platform._

/**
 * THE LOOP THAT CARRIES WORKFLOWS FORWARD (okay-persist's TestWorker;
 * workflow-worker): a stale timer costs a read and nothing else, two
 * workers produce ONE journal, and a worker that dies leaves the next
 * one able to carry on. JVM only: the activity row is `Async`.
 */
class TestWorker extends FunSuite {
  import DialogueFixtures._
  import WorkflowFixtures._
  import WorkerFixtures._

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1000L, id = "id", dice = 0.5)

  test("start drives a run to its sleep and ARMS the deadline") {
    val store = new MemoryStore
    val w = worker(store, store.topic("naps"))
    assertEquals(drive(w.start("n-1")), Worker.Progress.Sleeping(1000L + 60000L): Worker.Progress[String])
    assertEquals(Timers.over(store).armed, Map("n-1" -> 61000L))
  }

  test("tick before the deadline does nothing; after it, the run finishes") {
    val store = new MemoryStore
    val w = worker(store, store.topic("naps"))
    val _ = drive(w.start("n-1"))

    assertEquals(drive(w.tick(60000L)), Nil, "a deadline that has not passed fired")
    assertEquals(drive(w.tick(61000L)), List("n-1" -> (Worker.Progress.Finished("ada woke"): Worker.Progress[String])))
    // and a finished run is disarmed, so it is never picked up again
    assertEquals(Timers.over(store).armed, Map.empty[String, Long])
    assertEquals(drive(w.tick(999999L)), Nil)
  }

  test("a STALE timer costs a read and nothing else") {
    val store = new MemoryStore
    val w = worker(store, store.topic("naps"))
    val _ = drive(w.start("n-1"))
    val _ = drive(w.tick(61000L))            // finished and disarmed
    val journal = w.dialogue("n-1").journal

    // somebody re-arms a run that has moved on
    Timers.over(store).arm("n-1", 1L)
    assertEquals(drive(w.tick(999999L)).map(_._2), List(Worker.Progress.Finished("ada woke"): Worker.Progress[String]))

    // THE POINT: the journal did not gain an answer nobody asked for
    assertEquals(w.dialogue("n-1").journal, journal)
  }

  test("two workers on one dialogue produce ONE journal") {
    val store = new MemoryStore
    val t = store.topic("naps")
    val a = worker(store, t, "ada")
    val b = worker(store, t, "bob")

    // both start the same id from the same standing start
    assertEquals(drive(a.start("n-1")), Worker.Progress.Sleeping(61000L): Worker.Progress[String])
    // the second one found the first one's answer already in the log
    assertEquals(drive(b.start("n-1")), Worker.Progress.Sleeping(61000L): Worker.Progress[String])

    val j = a.dialogue("n-1").journal
    assertEquals(j.count(_.isRight), 1, s"the question was answered twice: $j")
    assertEquals(j.head, Right("ada"))
  }

  test("a worker that dies leaves the next one able to carry on") {
    val store = new MemoryStore
    val t = store.topic("naps")
    val _ = drive(worker(store, t).start("n-1"))

    // ---- this worker dies here, holding nothing
    val second = worker(store, t, answers = "never asked")
    assertEquals(drive(second.tick(61000L)), List("n-1" -> (Worker.Progress.Finished("ada woke"): Worker.Progress[String])),
      "the second worker re-asked the question instead of reading the log")
  }

  test("a run waiting on a SIGNAL is disarmed: the clock is not what wakes it") {
    val store = new MemoryStore
    def approve(w: W): String ! Rw = for {
      what <- w.pause("what?")
      by <- w.awaitSignal("approval")
    } yield s"$what by $by"
    val w = new Worker[String, String, String, P, Async](store.topic("signals"), "approve/1", Timers.over(store), say("the budget"))(approve)

    assertEquals(drive(w.start("a-1")), Worker.Progress.Waiting(Wf.Wait.Signal("approval")): Worker.Progress[String])
    assertEquals(Timers.over(store).armed, Map.empty[String, Long], "a signal-waiter was left on the clock's list")
  }

  // ==== the two rows, and why there are two =========================

  test("an ACTIVITY may reach outside; the PROGRAM may not") {
    val store = new MemoryStore
    var performed = 0
    // the oracle's work lives in the activity row: typed, not smuggled
    val w = new Worker[String, String, String, P, Async](store.topic("io"), "nap/1", Timers.over(store),
      say { performed += 1; "ada" })(nap)

    assertEquals(drive(w.start("n-1")), Worker.Progress.Sleeping(61000L): Worker.Progress[String])
    assertEquals(performed, 1)

    // and the PROGRAM's row still refuses it
    val e = compileErrors("""
      okay2.persist.Dialogue.workflow[String, String, String, okay2.async.Async](
        new okay2.persist.MemoryStore().topic("t"), "d", "p/1")(
          _ => okay2.pure[okay2.Shift[Any] with okay2.async.Async, String](""))""")
    assert(e.nonEmpty, "an Async-rowed workflow compiled")
    assert(e.contains("PERFORM AGAIN"), s"refused for the wrong reason: $e")
  }

  // ==== retries: the driver's business, not the program's ==========

  test("an activity that fails twice is retried, and the journal gains ONE answer") {
    val store = new MemoryStore
    val t = store.topic("retried")
    var attempts = 0
    val flaky: (String, Dialogue.Attempt) => String ! Async = (_, _) => Async {
      attempts += 1
      if (attempts < 3) throw new RuntimeException(s"attempt $attempts failed")
      "ada"
    }

    val w = new Worker[String, String, String, P, Async](t, "nap/1", Timers.over(store), Worker.retrying(Retry.immediate(5))(flaky))(nap)

    assertEquals(drive(w.start("n-1")), Worker.Progress.Sleeping(61000L): Worker.Progress[String])
    assertEquals(attempts, 3, "the driver did not retry")
    // THE POINT: the program asked once and was answered once
    assertEquals(w.dialogue("n-1").journal.count(_.isRight), 1)
    assertEquals(w.dialogue("n-1").journal.head, Right("ada"))
  }

  test("an exhausted policy gives up FOR NOW: nothing is journalled and the run stands") {
    val store = new MemoryStore
    val t = store.topic("exhausted")
    var attempts = 0
    val broken: (String, Dialogue.Attempt) => String ! Async = (_, _) => Async[String] {
      attempts += 1
      throw new RuntimeException("the service is down")
    }

    val w = new Worker[String, String, String, P, Async](t, "nap/1", Timers.over(store), Worker.retrying(Retry.immediate(2))(broken))(nap)

    intercept[RuntimeException](drive(w.start("n-1")))
    assertEquals(attempts, 3, "one attempt plus two retries")
    // nothing was written, so the run is exactly where it began
    assertEquals(w.dialogue("n-1").journal, Nil)

    // ---- the service comes back; a later worker finishes the run
    val ok = new Worker[String, String, String, P, Async](t, "nap/1", Timers.over(store), say("bob"))(nap)
    assertEquals(drive(ok.start("n-1")), Worker.Progress.Sleeping(61000L): Worker.Progress[String])
    assertEquals(ok.dialogue("n-1").journal.head, Right("bob"))
  }
}
