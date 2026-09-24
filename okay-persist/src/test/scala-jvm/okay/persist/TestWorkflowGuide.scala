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

  def drive[A](p: A ! Pure + Async)(using CanBlock): A =
    !.run(Async.run[A, Pure](p))

  // ---- the page's first block, verbatim

  def booking(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure = direct:
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
      oracle = Worker.retrying(Retry.immediate(3))(q => askTheUser(q)),
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
      case Worker.Progress.Broken(
        Dialogue.Diagnosis(Dialogue.Stopped.Mismatch(_, found, expected), _, _, _)) =>
        assertEquals(found, "booking/1")
        assertEquals(expected, "booking/2")
      case other => fail(s"a foreign journal was folded anyway: $other")
  }

  // ---- the page's cancellation block, verbatim

  def cancellable(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
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

  // ---- the page's bounded-history block, verbatim

  def stage(using w: Wf.Asks[String, String, Wf.Next[String, String], Pure])
      : Wf.Next[String, String] ! Delim + Pure = direct:
    val input = !w.pause("input")               // the SEED, on a continued run
    if input.length >= 4 then Wf.Next.Done(s"done:$input")
    else Wf.Next.Continue(input + "x")          // close this chapter, open the next

  test("the guide's bounded history: four chapters, a journal of one answer") {
    val store = MemoryStore()
    val worker = Worker[String, String, Wf.Next[String, String], Pure, Async](
      store.topic("stages"), "stage/1", Timers.over(store),
      _ => okay.async("a"), seedOf = Wf.Next.seed)(stage)

    assertEquals(drive(worker.start("s-1")),
      Worker.Progress.Finished(Wf.Next.Done("done:axxx")))
    assertEquals(worker.dialogue("s-1").journal.size, 1)
  }

  // ---- the page's child block, verbatim

  def paid(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
    direct:
      val id = !w.pause("start the payment run")   // the ACTIVITY spawns it
      val got = !w.awaitChild(id)                  // the run ENDS here
      s"paid: $got"

  def payment(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
    direct:
      val ref = !w.pause("charge")
      s"ok/$ref"

  test("the guide's child workflow: the spawn is an activity, the wait is durable") {
    val store = MemoryStore()
    val kids = Children.over(store)
    val childWorker = Worker[String, String, String, Pure, Async](
      store.topic("payments"), "payment/1", Timers.over(store),
      _ => okay.async("ref-9"), children = Some(kids))(payment)

    val parentWorker = Worker[String, String, String, Pure, Async](
      store.topic("bookings"), "paid/1", Timers.over(store),
      oracle = _ => okay.async:
        val id = "pay-1"
        val _ = drive(childWorker.start(id))
        kids.link(id, "b-1", "payment/1")
        id,
      children = Some(kids))(paid)

    assertEquals(drive(parentWorker.start("b-1")),
      Worker.Progress.Finished("paid: ok/ref-9"))
    assertEquals(kids.of("b-1").map((id, _, done) => (id, done)),
      List("pay-1" -> Some("ok/ref-9")))
  }

  // ---- the page's retirement block

  /** a program with a branch in the MIDDLE, so a journal written
   * before the branch has records after the point it would sit at */
  def staged(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
    direct:
      val city = !w.pause("city?")
      val promo = !w.patch("promo")
      val nights = !w.pause("nights?")
      if promo then s"$city/$nights/promo" else s"$city/$nights"

  test("the guide's retirement: three questions, three costs") {
    val store = MemoryStore()
    val t = store.topic("bookings")
    val worker = Worker[String, String, String, Pure, Async](
      t, "booking/1", Timers.over(store), _ => okay.async("Kyiv"))(booking)
    val _ = drive(worker.start("b-1"))

    // 1 · envelopes only: no body, no replay
    val c = Retire.census[Wf.Ans[String]](t)
    assertEquals(c.programs.keySet, Set("booking/1"))
    assert(!c.gone("booking/1"))
    assert(c.gone("booking/2"), "a program that never wrote here is not reported gone")

    // 2 · one replay each: who is still asking
    val states = !.run(Retire.states[Wf.Ask[String], Wf.Ans[String], String, Pure](
      c.programs("booking/1").ids.toList)(worker.dialogue))
    assert(states("b-1").isInstanceOf[Retire.State.Asking], s"got ${states("b-1")}")

    // 3 · a replay AND the body: which branches are still live
    val branches = !.run(Retire.patches[String, String, String, Pure](
      List("old-1" -> List(Right("Kyiv"), Right("2")),
           "new-1" -> List(Right("Lviv"), Left(Wf.SysA.Flag(true)), Right("3"))))(staged))
    assertEquals(branches("promo").skipped, Set("old-1"))
    assert(!branches("promo").oldHalfDead,
      "the else-branch was called dead with a run still on it")
  }

  // ---- the page's lease block

  test("the guide's lease: advisory, and the journal is one either way") {
    val store = MemoryStore()
    val t = store.topic("bookings")
    val leases = Leases.over(store)
    val w = Worker[String, String, String, Pure, Async](
      t, "booking/1", Timers.over(store), _ => okay.async("Kyiv"),
      leases = Some(leases), owner = "box-3")(booking)

    // somebody else is on it
    assert(leases.acquire("b-1", "box-9", System.currentTimeMillis() + 60_000L,
      System.currentTimeMillis()))
    assertEquals(drive(w.advance("b-1")), Worker.Progress.Busy("box-9"))
    assertEquals(w.dialogue("b-1").journal, Nil, "the busy worker drove anyway")
  }

  // ---- the page's "Running one" block, with every option on

  test("the guide's full worker: every option named on the page compiles") {
    val store = MemoryStore()
    val topic = store.topic("stages")
    val timers = Timers.over(store)
    val snaps = Snapshots(store, "stages__chapters")
    val sigs = Signals.over(store)
    val index = Statuses.over(store)
    val cancels = Cancels.over(store)
    val kids = Children.over(store)
    val leases = Leases.over(store)
    def oracle(q: String): String ! Async = okay.async("a" + q.take(0))

    val worker = Worker[String, String, Wf.Next[String, String], Pure, Async](
      topic, program = "stage/1", timers, q => oracle(q),
      snapshots     = Some(snaps),
      snapshotEvery = 64,
      signals       = Some(sigs),
      statuses      = Some(index),
      cancels       = Some(cancels),
      children      = Some(kids),
      seedOf        = Wf.Next.seed,
      continuations = 64,
      leases        = Some(leases),
      owner         = "box-3",
      resume        = Some(Resume()))(stage)

    // and it still runs: four chapters, a journal of one answer
    assertEquals(drive(worker.start("s-1")),
      Worker.Progress.Finished(Wf.Next.Done("done:axxx")))
    assertEquals(worker.dialogue("s-1").journal.size, 1)
  }
}
