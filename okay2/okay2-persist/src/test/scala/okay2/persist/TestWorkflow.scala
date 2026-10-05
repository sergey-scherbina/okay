package okay2.persist

import munit.FunSuite
import okay2.{!, pure}
import okay2.workflow.Wf
import okay2.codec.Schema

/** the workflow suites' shared fixtures */
object WorkflowFixtures {
  import DialogueFixtures._

  implicit val sysASchema: Schema[Wf.SysA] = Schema.derived
  implicit val ansSchema: Schema[Wf.Ans[String]] = Dialogue.answerSchema[String]

  type W = Wf.Asks[String, String, String, P]

  /** a drive that was expected to finish */
  def done[R](e: Either[Wf.Wait, R]): R = e match {
    case Right(r) => r
    case Left(w) => throw new AssertionError(s"expected the workflow to finish, it is waiting on $w")
  }

  def wf(t: Topic, id: String, program: String)(body: W => String ! Rw): Dialogue[Wf.Ask[String], Wf.Ans[String], String, P] =
    Dialogue.workflow[String, String, String, P](t, id, program)(body)

  /** an oracle that answers by the question alone */
  def answering(f: String => String): (String, Dialogue.Attempt) => String ! P =
    (q, _) => pure[P, String](f(q))

  /** an oracle that must not be asked */
  val never: (String, Dialogue.Attempt) => String ! P =
    (q, _) => throw new AssertionError(s"the oracle was asked again: $q")

  /** answer, then sleep a day, then finish */
  def overnight(w: W): String ! Rw = for {
    who <- w.pause("who?")
    _ <- w.sleep(86400000L)
  } yield s"$who slept"
}

/**
 * A DURABLE PROGRAM WITH A CLOCK AND A CHANGEABLE BRANCH (okay-persist's
 * TestWorkflow): the runtime's questions journalled in a topic, so the
 * clock survives a restart and `patch` decides once for the life of a
 * dialogue.
 */
class TestWorkflow extends FunSuite {
  import DialogueFixtures._
  import WorkflowFixtures._

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1700000000000L, id = "id-1", dice = 0.25)

  /** v1: a city, then the nights */
  def v1(w: W): String ! Rw = for {
    city <- w.pause("city?")
    n <- w.pause("nights?")
  } yield s"$city/$n"

  /** v2: the same, with a branch added BETWEEN the two questions */
  def v2(w: W): String ! Rw = for {
    city <- w.pause("city?")
    on <- w.patch("promo")
    n <- w.pause("nights?")
  } yield if (on) s"$city/$n/promo" else s"$city/$n"

  /** a program that stamps itself with the time */
  def stamped(w: W): String ! Rw = for {
    who <- w.pause("who?")
    t <- w.now
  } yield s"$who@$t"

  test("the clock is read ONCE and the reading outlives the process") {
    val t = new MemoryStore().topic("stamped")
    var reads = 0
    val counting: Wf.Runtime = new Wf.Runtime {
      def answer(q: Wf.Sys): Either[Wf.Wait, Wf.SysA] = {
        reads += 1
        Right(Wf.SysA.Millis(4242L))
      }
    }

    val first = done(!.run(wf(t, "s-1", "stamp/1")(stamped).runWorkflow(answering(_ => "ada"))(counting)))
    assertEquals(first, "ada@4242")
    assertEquals(reads, 1)

    // the process dies; a NEW one reads the same topic
    val second = done(!.run(wf(t, "s-1", "stamp/1")(stamped).runWorkflow(never)(counting)))
    assertEquals(second, "ada@4242", "the clock was read again after a restart")
    assertEquals(reads, 1, "the runtime was asked again after a restart")
  }

  test("patch through the durable journal: a run started under v1 keeps the old path") {
    val t = new MemoryStore().topic("patched")
    val old = done(!.run(wf(t, "p-1", "booking/1")(v1).runWorkflow(answering(q => if (q == "city?") "Kyiv" else "3"))))
    assertEquals(old, "Kyiv/3")

    // the deploy: the SAME journal, the new program, the name kept and
    // `patch` used instead — which is exactly what patch is for
    val after = !.run(wf(t, "p-1", "booking/1")(v2).at)
    assertEquals(after.map(_.finished), Right(Some("Kyiv/3")),
      "the patch ate the answer that followed it, or took the new branch")
  }

  test("patch: a dialogue that starts under v2 takes the new branch, durably") {
    val t = new MemoryStore().topic("patched2")
    val fresh = done(!.run(wf(t, "p-2", "booking/1")(v2).runWorkflow(answering(q => if (q == "city?") "Lviv" else "2"))))
    assertEquals(fresh, "Lviv/2/promo")

    // the decision is IN the log, so a third process agrees without
    // asking the runtime anything
    val d = wf(t, "p-2", "booking/1")(v2)
    assertEquals(d.journal.collect { case Left(f) => f }, List(Wf.SysA.Flag(true)))
    assertEquals(!.run(d.at).map(_.finished), Right(Some("Lviv/2/promo")))
  }

  test("a half-finished old journal goes live at the patch and finishes on the new branch") {
    val t = new MemoryStore().topic("patched3")
    // an old run that only answered the city
    val started = wf(t, "p-3", "booking/1")(v1)
    val _ = !.run(started.answer(Right("Kyiv")))
    assertEquals(started.journal, List(Right("Kyiv")))

    // the deploy, then the dialogue is driven to the end under v2
    val end = done(!.run(wf(t, "p-3", "booking/1")(v2).runWorkflow(answering(_ => "4"))))
    assertEquals(end, "Kyiv/4/promo")
    // and the decision was appended, between the two answers
    assertEquals(wf(t, "p-3", "booking/1")(v2).journal, List(Right("Kyiv"), Left(Wf.SysA.Flag(true)), Right("4")))
  }

  // ==== the engine's keystone, through the log =====================

  test("a durable workflow STOPS at a timer, and another process carries it on") {
    val t = new MemoryStore().topic("timed")
    val first = wf(t, "t-1", "night/1")(overnight)

    // the drive answers what it can and stops at the deadline
    !.run(first.runWorkflow(answering(_ => "ada"))) match {
      case Left(Wf.Wait.Until(when)) => assertEquals(when, 1700000000000L + 86400000L)
      case other => fail(s"expected a wait until the deadline, got $other")
    }

    // what it DID answer is durable: the name and the clock reading
    assertEquals(first.journal, List(Right("ada"), Left(Wf.SysA.Millis(1700000000000L))))

    // ---- this process dies; later the scheduler appends the answer
    val _ = !.run(first.answer(Left(Wf.SysA.Elapsed)))

    // a NEW process finishes it, asking the oracle nothing
    val end = done(!.run(wf(t, "t-1", "night/1")(overnight).runWorkflow(never)))
    assertEquals(end, "ada slept")
  }

  test("the deadline is in the LOG, so every process computes the same one") {
    val t = new MemoryStore().topic("timed2")
    val _ = !.run(wf(t, "t-2", "night/1")(overnight).runWorkflow(answering(_ => "ada")))

    // a second process, whose clock reads something else entirely
    val later: Wf.Runtime = Wf.Runtime.scripted(millis = 9999999L, id = "x", dice = 0.1)
    !.run(wf(t, "t-2", "night/1")(overnight).runWorkflow(never)(later)) match {
      case Left(Wf.Wait.Until(when)) =>
        assertEquals(when, 1700000000000L + 86400000L, "the second process moved the deadline")
      case other => fail(s"expected the same wait, got $other")
    }
  }

  test("a worker with NO activity row at all: the driver's row IS the program's") {
    val store = new MemoryStore
    val w = new Worker[String, String, String, P, P](store.topic("bare"), "nap/1", Timers.over(store),
      answering(_ => "ada"))(overnight)
    assertEquals(!.run(w.start("n-1")), Worker.Progress.Sleeping(1700000000000L + 86400000L): Worker.Progress[String])
  }
}
