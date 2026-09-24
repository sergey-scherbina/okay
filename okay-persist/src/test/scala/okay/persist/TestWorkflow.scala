package okay.persist

import munit.FunSuite
import okay.{!, +, Delim, Pure, Wf}
import okay.Direct.*
import okay.codec.Schema
import scala.language.implicitConversions

/**
 * A DURABLE PROGRAM WITH A CLOCK AND A CHANGEABLE BRANCH
 * (wf-durable-journal, 2026-09-17). `Wf` made the runtime's questions
 * journalled; this is the same thing when the journal is a topic, so
 * the clock survives a restart and `patch` decides once for the life
 * of a dialogue.
 */
class TestWorkflow extends FunSuite {

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_700_000_000_000L,
                                         id = "id-1", dice = 0.25)

  /**
   * v1: a city, then the nights. Since wf-direct-door a durable
   * workflow reads as ordinary straight-line code: the types are
   * named once in the signature and no call site repeats them.
   */
  def v1(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure = direct:
    val city = !w.pause("city?")
    val n = !w.pause("nights?")
    s"$city/$n"

  /** v2: the same, with a branch added BETWEEN the two questions */
  def v2(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure = direct:
    val city = !w.pause("city?")
    val on = !w.patch("promo")
    val n = !w.pause("nights?")
    if on then s"$city/$n/promo" else s"$city/$n"

  /** a program that stamps itself with the time */
  def stamped(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure = direct:
    val who = !w.pause("who?")
    s"$who@${!w.now}"

  /** a drive that was expected to finish: since
   * workflow-suspended-driver a durable drive may legitimately stop
   * at a timer or a signal, so the answer is an Either */
  def done[R](e: Either[Wf.Wait, R]): R = e match
    case Right(r) => r
    case Left(w) => fail(s"expected the workflow to finish, it is waiting on $w")

  def wf(t: Topic, id: String, program: String)
        (body: Wf.Asks[String, String, String, Pure] ?=> String ! Delim + Pure) =
    Dialogue.workflow[String, String, String, Pure](t, id, program)(body)

  test("the clock is read ONCE and the reading outlives the process") {
    val t = MemoryStore().topic("stamped")
    var reads = 0
    given counting: Wf.Runtime = new Wf.Runtime:
      def answer(q: Wf.Sys): Either[Wf.Wait, Wf.SysA] =
        reads += 1
        Right(Wf.SysA.Millis(4242L))

    val first = done(!.run(wf(t, "s-1", "stamp/1")(stamped)
      .runWorkflow(_ => okay.pure("ada"))))
    assertEquals(first, "ada@4242")
    assertEquals(reads, 1)

    // the process dies; a NEW one reads the same topic
    val second = done(!.run(wf(t, "s-1", "stamp/1")(stamped)
      .runWorkflow(_ => fail("the oracle was asked again"))))
    assertEquals(second, "ada@4242", "the clock was read again after a restart")
    assertEquals(reads, 1, "the runtime was asked again after a restart")
  }

  test("patch through the durable journal: a run started under v1 keeps the old path") {
    val t = MemoryStore().topic("patched")
    // the old run, finished under v1
    val old = done(!.run(wf(t, "p-1", "booking/1")(v1)
      .runWorkflow(q => okay.pure(if q == "city?" then "Kyiv" else "3"))))
    assertEquals(old, "Kyiv/3")

    // the deploy: the SAME journal, the new program. The `program`
    // field would stop the fold if it changed, so this is the case
    // where the author kept the name and used `patch` instead — which
    // is exactly what patch is for.
    val after = !.run(wf(t, "p-1", "booking/1")(v2).at)
    assertEquals(after.map(_.finished), Right(Some("Kyiv/3")),
      "the patch ate the answer that followed it, or took the new branch")
  }

  test("patch: a dialogue that starts under v2 takes the new branch, durably") {
    val t = MemoryStore().topic("patched2")
    val fresh = done(!.run(wf(t, "p-2", "booking/1")(v2)
      .runWorkflow(q => okay.pure(if q == "city?" then "Lviv" else "2"))))
    assertEquals(fresh, "Lviv/2/promo")

    // the decision is IN the log, so a third process agrees without
    // asking the runtime anything
    val d = wf(t, "p-2", "booking/1")(v2)
    assertEquals(d.journal.collect { case Left(f) => f },
      List(Wf.SysA.Flag(true)))
    assertEquals((!.run(d.at)).map(_.finished), Right(Some("Lviv/2/promo")))
  }

  test("a half-finished old journal goes live at the patch and finishes on the new branch") {
    val t = MemoryStore().topic("patched3")
    // an old run that only answered the city
    val started = wf(t, "p-3", "booking/1")(v1)
    val _ = !.run(started.answer(Right("Kyiv")))
    assertEquals(started.journal, List(Right("Kyiv")))

    // the deploy, then the dialogue is driven to the end under v2
    val end = done(!.run(wf(t, "p-3", "booking/1")(v2)
      .runWorkflow(_ => okay.pure("4"))))
    assertEquals(end, "Kyiv/4/promo")
    // and the decision was appended, between the two answers
    assertEquals(wf(t, "p-3", "booking/1")(v2).journal,
      List(Right("Kyiv"), Left(Wf.SysA.Flag(true)), Right("4")))
  }

  // ==== the engine's keystone, through the log =====================

  /** answer, then sleep a day, then finish */
  def overnight(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure = direct:
    val who = !w.pause("who?")
    !w.sleep(86_400_000L)
    s"$who slept"

  test("a durable workflow STOPS at a timer, and another process carries it on") {
    val t = MemoryStore().topic("timed")
    val first = wf(t, "t-1", "night/1")(overnight)

    // the drive answers what it can and stops at the deadline
    !.run(first.runWorkflow(_ => okay.pure("ada"))) match
      case Left(Wf.Wait.Until(when)) => assertEquals(when, 1_700_000_000_000L + 86_400_000L)
      case other => fail(s"expected a wait until the deadline, got $other")

    // what it DID answer is durable: the name and the clock reading
    assertEquals(first.journal,
      List(Right("ada"), Left(Wf.SysA.Millis(1_700_000_000_000L))))

    // ---- this process dies. Later, the scheduler sees the instant
    //      pass and appends the answer, as any other answer.
    val _ = !.run(first.answer(Left(Wf.SysA.Elapsed)))

    // a NEW process finishes it, asking the oracle nothing
    val end = done(!.run(wf(t, "t-1", "night/1")(overnight)
      .runWorkflow(_ => fail("the oracle was asked again"))))
    assertEquals(end, "ada slept")
  }

  test("the deadline is in the LOG, so every process computes the same one") {
    val t = MemoryStore().topic("timed2")
    val _ = !.run(wf(t, "t-2", "night/1")(overnight).runWorkflow(_ => okay.pure("ada")))

    // a second process, whose clock reads something else entirely
    val later: Wf.Runtime = Wf.Runtime.scripted(millis = 9_999_999L, id = "x", dice = 0.1)
    !.run(wf(t, "t-2", "night/1")(overnight).runWorkflow(_ =>
      fail("the oracle was asked again"))(using later)) match
      case Left(Wf.Wait.Until(when)) =>
        assertEquals(when, 1_700_000_000_000L + 86_400_000L,
          "the second process moved the deadline")
      case other => fail(s"expected the same wait, got $other")
  }

  test("a worker with NO activity row at all — the wart the complement form had") {
    // `Worker[..., Pure, Pure]`: the driver's row IS the program's.
    // Under the earlier `F + E` spelling this could not even be run —
    // `Pure + Pure` is `[X] =>> Nothing | Nothing`, which is not
    // `Nothing` — and under `Sub` it is ordinary, because
    // `Nothing <:< anything` (row-membership-crash).
    val store = MemoryStore()
    val w = Worker[String, String, String, Pure, Pure](
      store.topic("bare"), "nap/1", Timers.over(store),
      _ => okay.pure("ada"))(overnight)
    assertEquals(!.run(w.start("n-1")),
      Worker.Progress.Sleeping(1_700_000_000_000L + 86_400_000L))
  }
}
