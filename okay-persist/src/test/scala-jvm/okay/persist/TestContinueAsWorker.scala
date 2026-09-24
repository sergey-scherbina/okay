package okay.persist

import munit.FunSuite
import okay.{!, +, Async, CanBlock, Delim, Pure, Wf}
import okay.given_CanBlock
import okay.Direct.*
import okay.codec.Schema
import scala.language.implicitConversions

/**
 * BOUNDED HISTORY, END TO END (dialogue-continue-as, 2026-09-17).
 *
 * The first test is the whole feature in one assertion: a run that
 * has been through four chapters has a journal of ONE answer, so its
 * cold start replays one answer rather than everything that ever
 * happened to it. That is the cost `Wf.Next` exists to bound, and
 * chapters (`Snapshots`) cannot bound it — they cut the READING, not
 * the RUNNING.
 */
class TestContinueAsWorker extends FunSuite {

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_000L, id = "id", dice = 0.5)

  def drive[A](p: A ! Pure + Async)(using CanBlock): A =
    !.run(Async.run[A, Pure](p))

  type Out = Wf.Next[String, String]

  /** grows its input a letter at a time, one chapter per letter. The
   * FIRST pause is the input: a fresh run gets the oracle's answer, a
   * continued one gets the seed, and the program cannot tell. */
  def stage(using w: Wf.Asks[String, String, Out, Pure]): Out ! Delim + Pure =
    direct:
      val input = !w.pause("input")
      if input.length >= 4 then Wf.Next.Done(s"done:$input")
      else Wf.Next.Continue(input + "x")

  test("THE POINT: four chapters, a journal of one answer") {
    val store = MemoryStore()
    var asked = 0
    val w = Worker[String, String, Out, Pure, Async](
      store.topic("stages"), "stage/1", Timers.over(store),
      _ => okay.async { asked += 1; "a" },
      seedOf = Wf.Next.seed)(stage)

    assertEquals(drive(w.start("s-1")), Worker.Progress.Finished(Wf.Next.Done("done:axxx")))

    val d = w.dialogue("s-1")
    assertEquals(d.journal.size, 1, s"the history was not bounded: ${d.journal}")
    assertEquals(d.journal, List(Right("axxx")))
    assertEquals(d.recovered.accepted, 4, "the record count should span every chapter")

    // and the outside world was consulted once in the whole run,
    // however many chapters it took
    assertEquals(asked, 1)
  }

  test("a run that only ever continues hands back instead of spinning") {
    val store = MemoryStore()
    def forever(using w: Wf.Asks[String, String, Out, Pure]): Out ! Delim + Pure =
      direct:
        val input = !w.pause("input")
        Wf.Next.Continue(input + "x")

    val w = Worker[String, String, Out, Pure, Async](
      store.topic("spin"), "forever/1", Timers.over(store),
      _ => okay.async("a"),
      seedOf = Wf.Next.seed, continuations = 3)(forever)

    assertEquals(drive(w.start("s-1")), Worker.Progress.Continued(3))
    // nothing is wrong with it: the next call carries it on
    assertEquals(drive(w.advance("s-1")), Worker.Progress.Continued(3))
    assertEquals(w.dialogue("s-1").journal.size, 1)
  }

  test("a workflow that never continues is untouched by any of this") {
    val store = MemoryStore()
    def plain(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
      direct:
        val who = !w.pause("who?")
        s"$who woke"
    val w = Worker[String, String, String, Pure, Async](
      store.topic("plain"), "plain/1", Timers.over(store), _ => okay.async("ada"))(plain)

    assertEquals(drive(w.start("p-1")), Worker.Progress.Finished("ada woke"))
    assertEquals(w.dialogue("p-1").journal, List(Right("ada")))
  }
}
