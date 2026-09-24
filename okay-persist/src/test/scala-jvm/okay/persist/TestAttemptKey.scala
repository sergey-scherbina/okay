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
 * THE IDEMPOTENCY KEY REACHES THE ORACLE (worker-oracle-attempt,
 * 2026-09-17).
 *
 * `Dialogue.Attempt(id, index)` was documented as what a
 * non-idempotent external call needs, and then dropped by `askingIn`
 * — so nobody driving through a `Worker` could use it. It arrives as
 * CONTEXT now, which is what let the change happen without touching
 * the thirty-odd call sites that do not want it.
 *
 * The second test is the one that makes it a KEY rather than a
 * number: a question re-asked after a failure carries the SAME
 * position, because the position is the journal's own and nothing was
 * journalled.
 */
class TestAttemptKey extends FunSuite {

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_000L, id = "id", dice = 0.5)

  def drive[A](p: A ! Pure + Async)(using CanBlock): A =
    !.run(Async.run[A, Pure](p))

  def two(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
    direct:
      val a = !w.pause("1?")
      val b = !w.pause("2?")
      s"$a$b"

  test("an oracle can ask where it is, and the answer is the journal's position") {
    val store = MemoryStore()
    var seen = List.empty[(String, String, Int)]
    val w = Worker[String, String, String, Pure, Async](
      store.topic("keys"), "two/1", Timers.over(store),
      q => okay.async:
        val at = summon[Dialogue.Attempt]
        seen = seen :+ (q, at.id, at.index)
        "x")(two)

    assertEquals(drive(w.start("k-1")), Worker.Progress.Finished("xx"))
    assertEquals(seen, List(("1?", "k-1", 0), ("2?", "k-1", 1)))
  }

  test("THE POINT: a question re-asked after a failure carries the SAME position") {
    val store = MemoryStore()
    val t = store.topic("keys")
    var seen = List.empty[Int]
    var fail = true

    def worker = Worker[String, String, String, Pure, Async](
      t, "two/1", Timers.over(store),
      q => okay.async:
        val at = summon[Dialogue.Attempt]
        if q == "2?" then
          seen = seen :+ at.index
          if fail then throw new RuntimeException("the service is down")
        "x")(two)

    // the first drive answers "1?", then the service is down for "2?"
    val _ = intercept[RuntimeException](drive(worker.start("k-1")))
    assertEquals(seen, List(1))
    // nothing was journalled for the failed question
    assertEquals(worker.dialogue("k-1").journal, List(Right("x")))

    // the process dies and another one picks the run up
    fail = false
    assertEquals(drive(worker.advance("k-1")), Worker.Progress.Finished("xx"))
    assertEquals(seen, List(1, 1),
      "the re-ask carried a different position, so it is not a key")
  }

  test("every attempt at ONE question carries one position, retries included") {
    val store = MemoryStore()
    var seen = List.empty[Int]
    var left = 2
    val w = Worker[String, String, String, Pure, Async](
      store.topic("keys"), "two/1", Timers.over(store),
      Worker.retrying(okay.Retry.immediate(5))(q =>
        okay.async:
          val at = summon[Dialogue.Attempt]
          if q == "2?" then
            seen = seen :+ at.index
            if left > 0 then { left -= 1; throw new RuntimeException("flaky") }
          "x"))(two)

    assertEquals(drive(w.start("k-1")), Worker.Progress.Finished("xx"))
    assertEquals(seen, List(1, 1, 1), "the retries did not share one key")
  }
}
