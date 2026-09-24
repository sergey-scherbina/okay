package okay.persist

import munit.FunSuite
import okay.{!, +, Async, CanBlock, Delim, Pure, Wf}
import okay.given_CanBlock
import okay.given_Scheduler
import okay.Direct.*
import okay.codec.Schema
import scala.language.implicitConversions

/**
 * ONE RUN'S FAILURE MUST NOT TAKE THE BATCH (worker-tick-isolation,
 * 2026-09-17).
 *
 * An oracle whose retry policy is exhausted THROWS, on purpose: the
 * drive ends with nothing journalled, the run still stands at its
 * question, and a later worker asks again. That contract is right for
 * `advance`, which is about one run and whose caller asked about one
 * run.
 *
 * `tick` is about ALL of them, and there the same throw is a
 * different thing: it ends the pass, skips every run after the failing
 * one, and DISCARDS the results of the runs already advanced — so the
 * caller cannot even tell what happened before the failure. That is
 * the shape `look-before-driving` fixed for an unreadable journal, and
 * this is the other half of it.
 */
class TestTickIsolation extends FunSuite {

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_000L, id = "id", dice = 0.5)

  def drive[A](p: A ! Pure + Async)(using CanBlock): A =
    !.run(Async.run[A, Pure](p))

  def nap(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
    direct:
      val who = !w.pause("who?")
      !w.sleep(60_000L)
      val what = !w.pause("what?")
      s"$who/$what"

  test("THE POINT: a run whose oracle throws does not take the batch with it") {
    val store = MemoryStore()
    val t = store.topic("naps")
    val timers = Timers.over(store)

    // the first question is answered for everybody; the SECOND one,
    // asked after the nap, is where "bad" fails for ever
    def oracle(id: String)(q: String): String ! Async = okay.async:
      if q == "what?" && id == "bad" then throw new RuntimeException("the service is down")
      else "ok"

    def worker(id: String) = Worker[String, String, String, Pure, Async](
      t, "nap/1", timers, oracle(id),
      isolate = Some(Worker.isolating))(nap)

    // two runs, both asleep, both due
    val _ = drive(worker("good").start("good"))
    val _ = drive(worker("bad").start("bad"))
    assertEquals(timers.armed.keySet, Set("good", "bad"))

    // one worker drives both, and `due` hands them back sorted, so
    // "bad" is woken first. Its second question throws; "good"'s must
    // still be answered, and the caller must be TOLD which was which.
    var asked = 0
    val w = Worker[String, String, String, Pure, Async](
      t, "nap/1", timers,
      q => okay.async:
        if q != "what?" then "ok"
        else
          asked += 1
          if asked == 1 then throw new RuntimeException("the service is down") else "ok",
      isolate = Some(Worker.isolating))(nap)

    val out = drive(w.tick(61_000L)).toMap
    assertEquals(out.keySet, Set("good", "bad"), s"the pass lost a run: $out")
    assert(out("bad").isInstanceOf[Worker.Progress.Failed], s"got ${out("bad")}")
    assertEquals(out("good"), Worker.Progress.Finished("ok/ok"),
      "the failing run took the one after it down")

    // and the failed run is exactly where it was: nothing journalled
    assertEquals(w.dialogue("bad").journal.count(_.isRight), 1,
      "a throw left something in the journal")
  }

  test("without isolation the throw still escapes, which is `advance`'s contract") {
    val store = MemoryStore()
    val t = store.topic("naps")
    val w = Worker[String, String, String, Pure, Async](
      t, "nap/1", Timers.over(store),
      q => okay.async { if q == "what?" then throw new RuntimeException("down") else "ok" })(nap)

    val _ = drive(w.start("n-1"))
    // one run, one caller, one question: the throw belongs to them
    val _ = intercept[RuntimeException](drive(w.tick(61_000L)))
  }
}
