package okay.persist

import munit.FunSuite
import okay.{!, +, Async, CanBlock, Delim, Pure, Retry, Wf}
import okay.Direct.*
import okay.codec.Schema
import okay.given_CanBlock
import okay.given_Scheduler
import okay.given_Timer
import scala.language.implicitConversions

/**
 * THE GUIDE, COMPILED (workflow-docs, 2026-09-17).
 *
 * `docs/durable-workflows.md` opens with a five-line workflow and
 * closes with the twelve lines that run one. Both are HERE, so the
 * page cannot drift away from the library: if this suite goes red,
 * the documentation is wrong, and that is the only way to keep a
 * guide honest.
 *
 * JVM only for the same reason the other worker suites are: driving
 * an `Async` activity row to a value needs `CanBlock`.
 */
class TestWorkflowGuide extends FunSuite {

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_700_000_000_000L,
                                         id = "id-1", dice = 0.25)

  def drive[A](p: A ! (Pure + Async))(using CanBlock): A =
    !.run(Async.run[A, Pure](p))

  // ---- the page's first block, verbatim

  def booking(using w: Wf.Asks[String, String, String, Pure]): String ! (Delim + Pure) = direct:
    val city = !w.pause("which city?")           // the WORLD answers
    val start = !w.now                           // the RUNTIME answers, once
    !w.sleep(24 * 3600 * 1000L)                  // the run ENDS here and resumes tomorrow
    val ok = !w.awaitSignal("payment")           // somebody sends this, whenever
    if !w.patch("promo") then s"$city/promo/$start/$ok" else s"$city/$start/$ok"

  test("the guide's workflow does what the guide says, step by step") {
    val store = MemoryStore()
    val timers = Timers.over(store)
    val sigs = Signals.over(store)
    val index = Statuses.over(store)

    var asked = List.empty[String]
    def askTheUser(q: String): String ! Async = okay.async:
      asked = asked :+ q
      "Kyiv"

    val worker = Worker[String, String, String, Pure, Async](
      store.topic("bookings"), program = "booking/1", timers,
      oracle = Worker.retrying(Retry.immediate(3))(askTheUser),
      signals = Some(sigs), statuses = Some(index))(booking)

    // 1 · it runs until it waits, and NOTHING blocks
    assertEquals(drive(worker.start("booking-42")),
      Worker.Progress.Sleeping(1_700_000_000_000L + 24 * 3600 * 1000L))
    assertEquals(asked, List("which city?"), "the question was asked more than once")

    // 2 · the deadline is in a topic, not in memory
    assertEquals(timers.armed, Map("booking-42" -> (1_700_000_000_000L + 86_400_000L)))

    // 3 · the process may die here. A day later, a tick wakes it — and
    //     it stops again, this time on the signal
    assertEquals(drive(worker.tick(1_700_000_000_000L + 86_400_001L)),
      List("booking-42" -> Worker.Progress.Waiting(Wf.Wait.Signal("payment"))))
    // waiting on somebody's action is not waiting on the clock
    assertEquals(timers.armed, Map.empty[String, Long])

    // 4 · the dashboard answers "who is blocked on payment" with a read
    assertEquals(index.waitingOn("payment").map(_.id), List("booking-42"))

    // 5 · the signal arrives, from anywhere, at any time
    val _ = sigs.send("booking-42", "payment", "ok")
    assertEquals(drive(worker.advance("booking-42")),
      Worker.Progress.Finished("Kyiv/promo/1700000000000/ok"))

    // 6 · and the oracle was asked exactly once in the whole life of
    //     the run, however many times the program was replayed
    assertEquals(asked, List("which city?"))
  }

  test("the guide's claim about operational data: delete the timers, lose nothing") {
    val store = MemoryStore()
    val t = store.topic("bookings")
    val w1 = Worker[String, String, String, Pure, Async](
      t, "booking/1", Timers.over(store), _ => okay.async("Kyiv"))(booking)
    val _ = drive(w1.start("b-1"))

    // a worker over the same journal with a DIFFERENT (empty) timer
    // topic: the run is still exactly where its journal says
    val w2 = Worker[String, String, String, Pure, Async](
      t, "booking/1", Timers.over(MemoryStore()),
      _ => fail("the oracle was asked again"))(booking)
    assertEquals(drive(w2.advance("b-1")),
      Worker.Progress.Sleeping(1_700_000_000_000L + 86_400_000L))
  }

  test("the guide's claim about a changed program: the name stops the fold") {
    val store = MemoryStore()
    val t = store.topic("bookings")
    val w1 = Worker[String, String, String, Pure, Async](
      t, "booking/1", Timers.over(store), _ => okay.async("Kyiv"))(booking)
    val _ = drive(w1.start("b-1"))

    // the same journal, a program that calls itself something else
    val w2 = Worker[String, String, String, Pure, Async](
      t, "booking/2", Timers.over(store), _ => okay.async("Lviv"))(booking)
    drive(w2.advance("b-1")) match
      case Worker.Progress.Broken(Dialogue.Stopped.Mismatch(_, found, expected)) =>
        assertEquals(found, "booking/1")
        assertEquals(expected, "booking/2")
      case other => fail(s"a foreign journal was folded anyway: $other")
  }

  // ---- the page's cancellation block, verbatim

  def cancellable(using w: Wf.Asks[String, String, String, Pure]): String ! (Delim + Pure) =
    direct:
      val city = !w.pause("which city?")
      !w.sleep(24 * 3600 * 1000L)
      !w.cancelled match                      // the author decides WHERE
        case Some(why) => s"released $city: $why"
        case None      => s"confirmed $city"

  test("the guide's cancellation: cooperative, and the decision is replayed") {
    val store = MemoryStore()
    val cancels = Cancels.over(store)
    val worker = Worker[String, String, String, Pure, Async](
      store.topic("bookings"), "booking/1", Timers.over(store),
      _ => okay.async("Kyiv"), cancels = Some(cancels))(cancellable)

    val _ = drive(worker.start("b-1"))
    assert(worker.cancel("b-1", "customer withdrew"))
    assertEquals(drive(worker.tick(1_700_000_000_000L + 86_400_001L)),
      List("b-1" -> Worker.Progress.Finished("released Kyiv: customer withdrew")))

    // and the page's claim: withdrawing afterwards changes nothing,
    // because the run decided from its journal
    cancels.withdraw("b-1")
    assertEquals(drive(worker.advance("b-1")),
      Worker.Progress.Finished("released Kyiv: customer withdrew"))
  }
}
