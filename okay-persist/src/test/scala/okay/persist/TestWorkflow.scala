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
  def v1(using w: Wf.Asks[String, String, String, Pure]): String ! (Delim + Pure) = direct:
    val city = !w.pause("city?")
    val n = !w.pause("nights?")
    s"$city/$n"

  /** v2: the same, with a branch added BETWEEN the two questions */
  def v2(using w: Wf.Asks[String, String, String, Pure]): String ! (Delim + Pure) = direct:
    val city = !w.pause("city?")
    val on = !w.patch("promo")
    val n = !w.pause("nights?")
    if on then s"$city/$n/promo" else s"$city/$n"

  /** a program that stamps itself with the time */
  def stamped(using w: Wf.Asks[String, String, String, Pure]): String ! (Delim + Pure) = direct:
    val who = !w.pause("who?")
    s"$who@${!w.now}"

  def wf(t: Topic, id: String, program: String)
        (body: Wf.Asks[String, String, String, Pure] ?=> String ! (Delim + Pure)) =
    Dialogue.workflow[String, String, String, Pure](t, id, program)(body)

  test("the clock is read ONCE and the reading outlives the process") {
    val t = MemoryStore().topic("stamped")
    var reads = 0
    given counting: Wf.Runtime = new Wf.Runtime:
      def answer(q: Wf.Sys): Wf.SysA =
        reads += 1
        Wf.SysA.Millis(4242L)

    val first = !.run(wf(t, "s-1", "stamp/1")(stamped)
      .run(Dialogue.asking[String, String, Pure](_ => okay.pure("ada"))))
    assertEquals(first, "ada@4242")
    assertEquals(reads, 1)

    // the process dies; a NEW one reads the same topic
    val second = !.run(wf(t, "s-1", "stamp/1")(stamped)
      .run(Dialogue.asking[String, String, Pure](_ => fail("the oracle was asked again"))))
    assertEquals(second, "ada@4242", "the clock was read again after a restart")
    assertEquals(reads, 1, "the runtime was asked again after a restart")
  }

  test("patch through the durable journal: a run started under v1 keeps the old path") {
    val t = MemoryStore().topic("patched")
    // the old run, finished under v1
    val old = !.run(wf(t, "p-1", "booking/1")(v1)
      .run(Dialogue.asking[String, String, Pure](q =>
        okay.pure(if q == "city?" then "Kyiv" else "3"))))
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
    val fresh = !.run(wf(t, "p-2", "booking/1")(v2)
      .run(Dialogue.asking[String, String, Pure](q =>
        okay.pure(if q == "city?" then "Lviv" else "2"))))
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
    val done = !.run(wf(t, "p-3", "booking/1")(v2)
      .run(Dialogue.asking[String, String, Pure](_ => okay.pure("4"))))
    assertEquals(done, "Kyiv/4/promo")
    // and the decision was appended, between the two answers
    assertEquals(wf(t, "p-3", "booking/1")(v2).journal,
      List(Right("Kyiv"), Left(Wf.SysA.Flag(true)), Right("4")))
  }
}
