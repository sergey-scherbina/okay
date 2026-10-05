package okay2.persist

import munit.FunSuite
import okay2.{!}
import okay2.workflow.Wf
import okay2.async.{Async, Retry}
import okay2.platform._

/**
 * THE GUIDE, COMPILED (okay-persist's TestWorkflowGuide): the durable
 * workflow guide's blocks, run — a booking that waits a day and then on
 * a signal, cancellation, bounded history, a child, retirement, a lease,
 * and a worker with every option named. docs/okay2.md section 37 pins
 * its lines here.
 */
class TestWorkflowGuide extends FunSuite {
  import DialogueFixtures._
  import WorkflowFixtures._
  import WorkerFixtures._

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1700000000000L, id = "id-1", dice = 0.25)

  def booking(w: W): String ! Rw = for {
    city <- w.pause("which city?")           // the WORLD answers
    start <- w.now                           // the RUNTIME answers, once
    _ <- w.sleep(24 * 3600 * 1000L)          // the run ENDS here and resumes tomorrow
    ok <- w.awaitSignal("payment")           // somebody sends this, whenever
    promo <- w.patch("promo")
  } yield if (promo) s"$city/promo/$start/$ok" else s"$city/$start/$ok"

  test("the guide's workflow does what the guide says, step by step") {
    val store = new MemoryStore
    val timers = Timers.over(store)
    val sigs = Signals.over(store)
    val index = Statuses.over(store)

    var asked = List.empty[String]
    val askTheUser: (String, Dialogue.Attempt) => String ! Async = (q, _) => Async { asked = asked :+ q; "Kyiv" }

    val worker = new Worker[String, String, String, P, Async](store.topic("bookings"), program = "booking/1", timers,
      oracle = Worker.retrying(Retry.immediate(3))(askTheUser),
      signals = Some(sigs), statuses = Some(index))(booking)

    // 1 · it runs until it waits, and NOTHING blocks
    assertEquals(drive(worker.start("booking-42")), Worker.Progress.Sleeping(1700000000000L + 24 * 3600 * 1000L): Worker.Progress[String])
    assertEquals(asked, List("which city?"), "the question was asked more than once")

    // 2 · the deadline is in a topic, not in memory
    assertEquals(timers.armed, Map("booking-42" -> (1700000000000L + 86400000L)))

    // 3 · a day later, a tick wakes it — and it stops again, on the signal
    assertEquals(drive(worker.tick(1700000000000L + 86400001L)),
      List("booking-42" -> (Worker.Progress.Waiting(Wf.Wait.Signal("payment")): Worker.Progress[String])))
    // waiting on somebody's action is not waiting on the clock
    assertEquals(timers.armed, Map.empty[String, Long])

    // 4 · the dashboard answers "who is blocked on payment" with a read
    assertEquals(index.waitingOn("payment").map(_.id), List("booking-42"))

    // 5 · the signal arrives, from anywhere, at any time
    val _ = sigs.send("booking-42", "payment", "ok")
    assertEquals(drive(worker.advance("booking-42")), Worker.Progress.Finished("Kyiv/promo/1700000000000/ok"): Worker.Progress[String])

    // 6 · the oracle was asked exactly once in the whole life of the run
    assertEquals(asked, List("which city?"))
  }

  test("the guide's claim about operational data: delete the timers, lose nothing") {
    val store = new MemoryStore
    val t = store.topic("bookings")
    val _ = drive(new Worker[String, String, String, P, Async](t, "booking/1", Timers.over(store), say("Kyiv"))(booking).start("b-1"))

    // a worker over the same journal with a DIFFERENT (empty) timer topic
    val w2 = new Worker[String, String, String, P, Async](t, "booking/1", Timers.over(new MemoryStore),
      (q, _) => Async[String](fail(s"the oracle was asked again: $q")))(booking)
    assertEquals(drive(w2.advance("b-1")), Worker.Progress.Sleeping(1700000000000L + 86400000L): Worker.Progress[String])
  }

  test("the guide's claim about a changed program: the name stops the fold") {
    val store = new MemoryStore
    val t = store.topic("bookings")
    val _ = drive(new Worker[String, String, String, P, Async](t, "booking/1", Timers.over(store), say("Kyiv"))(booking).start("b-1"))

    // the same journal, a program that calls itself something else
    val w2 = new Worker[String, String, String, P, Async](t, "booking/2", Timers.over(store), say("Lviv"))(booking)
    drive(w2.advance("b-1")) match {
      case Worker.Progress.Broken(Dialogue.Diagnosis(Dialogue.Stopped.Mismatch(_, found, expected), _, _, _)) =>
        assertEquals(found, "booking/1")
        assertEquals(expected, "booking/2")
      case other => fail(s"a foreign journal was folded anyway: $other")
    }
  }

  def cancellable(w: W): String ! Rw = for {
    city <- w.pause("which city?")
    _ <- w.sleep(24 * 3600 * 1000L)
    why <- w.cancelled                      // the author decides WHERE
  } yield why.fold(s"confirmed $city")(r => s"released $city: $r")

  test("the guide's cancellation: cooperative, and the decision is replayed") {
    val store = new MemoryStore
    val cancels = Cancels.over(store)
    val worker = new Worker[String, String, String, P, Async](store.topic("bookings"), "booking/1", Timers.over(store),
      say("Kyiv"), cancels = Some(cancels))(cancellable)

    val _ = drive(worker.start("b-1"))
    assert(worker.cancel("b-1", "customer withdrew"))
    assertEquals(drive(worker.tick(1700000000000L + 86400001L)),
      List("b-1" -> (Worker.Progress.Finished("released Kyiv: customer withdrew"): Worker.Progress[String])))

    // withdrawing afterwards changes nothing: the run decided from its journal
    cancels.withdraw("b-1")
    assertEquals(drive(worker.advance("b-1")), Worker.Progress.Finished("released Kyiv: customer withdrew"): Worker.Progress[String])
  }

  type Out = Wf.Next[String, String]

  def stage(w: Wf.Asks[String, String, Out, P]): Out ! Rw =
    w.pause("input").map { input =>                 // the SEED, on a continued run
      if (input.length >= 4) Wf.Next.Done(s"done:$input")
      else Wf.Next.Continue(input + "x")            // close this chapter, open the next
    }

  val seed: Out => Option[String] = n => Wf.Next.seed(n)

  test("the guide's bounded history: four chapters, a journal of one answer") {
    val store = new MemoryStore
    val worker = new Worker[String, String, Out, P, Async](store.topic("stages"), "stage/1", Timers.over(store),
      say("a"), seedOf = seed)(stage)

    assertEquals(drive(worker.start("s-1")), Worker.Progress.Finished(Wf.Next.Done("done:axxx")): Worker.Progress[Out])
    assertEquals(worker.dialogue("s-1").journal.size, 1)
  }

  def paid(w: W): String ! Rw = for {
    id <- w.pause("start the payment run")   // the ACTIVITY spawns it
    got <- w.awaitChild(id)                  // the run ENDS here
  } yield s"paid: $got"

  def payment(w: W): String ! Rw = w.pause("charge").map(ref => s"ok/$ref")

  test("the guide's child workflow: the spawn is an activity, the wait is durable") {
    val store = new MemoryStore
    val kids = Children.over(store)
    val childWorker = new Worker[String, String, String, P, Async](store.topic("payments"), "payment/1", Timers.over(store),
      say("ref-9"), children = Some(kids))(payment)

    val parentWorker = new Worker[String, String, String, P, Async](store.topic("bookings"), "paid/1", Timers.over(store),
      oracle = (_, _) => Async {
        val id = "pay-1"
        val _ = drive(childWorker.start(id))
        kids.link(id, "b-1", "payment/1")
        id
      },
      children = Some(kids))(paid)

    assertEquals(drive(parentWorker.start("b-1")), Worker.Progress.Finished("paid: ok/ref-9"): Worker.Progress[String])
    assertEquals(kids.of("b-1").map { case (id, _, done) => (id, done) }, List("pay-1" -> Some("ok/ref-9")))
  }

  /** a branch in the MIDDLE, so a journal written before the branch has
   * records after the point it would sit at */
  def staged(w: W): String ! Rw = for {
    city <- w.pause("city?")
    promo <- w.patch("promo")
    nights <- w.pause("nights?")
  } yield if (promo) s"$city/$nights/promo" else s"$city/$nights"

  test("the guide's retirement: three questions, three costs") {
    val store = new MemoryStore
    val t = store.topic("bookings")
    val worker = new Worker[String, String, String, P, Async](t, "booking/1", Timers.over(store), say("Kyiv"))(booking)
    val _ = drive(worker.start("b-1"))

    // 1 · envelopes only: no body, no replay
    val c = Retire.census[Wf.Ans[String]](t)
    assertEquals(c.programs.keySet, Set("booking/1"))
    assert(!c.gone("booking/1"))
    assert(c.gone("booking/2"), "a program that never wrote here is not reported gone")

    // 2 · one replay each: who is still asking
    val states = !.run(Retire.states[Wf.Ask[String], Wf.Ans[String], String, P](c.programs("booking/1").ids.toList)(worker.dialogue))
    assert(states("b-1").isInstanceOf[Retire.State.Asking], s"got ${states("b-1")}")

    // 3 · a replay AND the body: which branches are still live
    val old: List[Wf.Ans[String]] = List(Right("Kyiv"), Right("2"))
    val fresh: List[Wf.Ans[String]] = List(Right("Lviv"), Left(Wf.SysA.Flag(true)), Right("3"))
    val branches = !.run(Retire.patches[String, String, String, P](List("old-1" -> old, "new-1" -> fresh))(staged))
    assertEquals(branches("promo").skipped, Set("old-1"))
    assert(!branches("promo").oldHalfDead, "the else-branch was called dead with a run still on it")
  }

  test("the guide's lease: advisory, and the journal is one either way") {
    val store = new MemoryStore
    val leases = Leases.over(store)
    val w = new Worker[String, String, String, P, Async](store.topic("bookings"), "booking/1", Timers.over(store), say("Kyiv"),
      leases = Some(leases), owner = "box-3")(booking)

    // somebody else is on it
    assert(leases.acquire("b-1", "box-9", System.currentTimeMillis() + 60000L, System.currentTimeMillis()))
    assertEquals(drive(w.advance("b-1")), Worker.Progress.Busy("box-9"): Worker.Progress[String])
    assertEquals(w.dialogue("b-1").journal, Nil, "the busy worker drove anyway")
  }

  test("the guide's full worker: every option named on the page compiles") {
    val store = new MemoryStore
    val topic = store.topic("stages")
    val timers = Timers.over(store)
    val snaps = Snapshots(store, "stages__chapters")
    val sigs = Signals.over(store)
    val index = Statuses.over(store)
    val cancels = Cancels.over(store)
    val kids = Children.over(store)
    val leases = Leases.over(store)

    val worker = new Worker[String, String, Out, P, Async](
      topic, program = "stage/1", timers, say("a"),
      snapshots     = Some(snaps),
      snapshotEvery = 64,
      signals       = Some(sigs),
      statuses      = Some(index),
      cancels       = Some(cancels),
      children      = Some(kids),
      seedOf        = seed,
      continuations = 64,
      leases        = Some(leases),
      owner         = "box-3",
      resume        = Some(new Resume[Wf.Ask[String], Wf.Ans[String], Out, P]()))(stage)

    // and it still runs: four chapters, a journal of one answer
    assertEquals(drive(worker.start("s-1")), Worker.Progress.Finished(Wf.Next.Done("done:axxx")): Worker.Progress[Out])
    assertEquals(worker.dialogue("s-1").journal.size, 1)
  }
}
