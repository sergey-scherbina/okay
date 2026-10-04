package okay2.persist

import munit.FunSuite
import okay2.{!, Wf}
import okay2.async.Async
import okay2.platform._

/**
 * ONE RUN'S FAILURE MUST NOT TAKE THE BATCH (okay-persist's
 * TestTickIsolation; worker-tick-isolation): `tick` is about ALL the
 * due runs, and an exhausted oracle's throw there would end the pass,
 * skip every run after the failing one and discard the results already
 * advanced.
 */
class TestTickIsolation extends FunSuite {
  import DialogueFixtures._
  import WorkflowFixtures._
  import WorkerFixtures._

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1000L, id = "id", dice = 0.5)

  def twoStep(w: W): String ! Rw = for {
    who <- w.pause("who?")
    _ <- w.sleep(60000L)
    what <- w.pause("what?")
  } yield s"$who/$what"

  test("THE POINT: a run whose oracle throws does not take the batch with it") {
    val store = new MemoryStore
    val t = store.topic("naps")
    val timers = Timers.over(store)

    val _ = drive(new Worker[String, String, String, P, Async](t, "nap/1", timers, say("ok"), isolate = Some(Worker.isolating))(twoStep).start("good"))
    val _ = drive(new Worker[String, String, String, P, Async](t, "nap/1", timers, say("ok"), isolate = Some(Worker.isolating))(twoStep).start("bad"))
    assertEquals(timers.armed.keySet, Set("good", "bad"))

    // the first "what?" asked (the due ids are sorted: "bad" first) fails
    var asked = 0
    val w = new Worker[String, String, String, P, Async](t, "nap/1", timers,
      (q, _) => Async {
        if (q != "what?") "ok"
        else {
          asked += 1
          if (asked == 1) throw new RuntimeException("the service is down") else "ok"
        }
      },
      isolate = Some(Worker.isolating))(twoStep)

    val out = drive(w.tick(61000L)).toMap
    assertEquals(out.keySet, Set("good", "bad"), s"the pass lost a run: $out")
    assert(out("bad").isInstanceOf[Worker.Progress.Failed], s"got ${out("bad")}")
    assertEquals(out("good"), Worker.Progress.Finished("ok/ok"): Worker.Progress[String], "the failing run took the one after it down")

    // and the failed run is exactly where it was: its throw journalled
    // nothing past the first answer
    assertEquals(w.dialogue("bad").journal.count(_.isRight), 1, "a throw left something in the journal")
  }

  test("without isolation the throw still escapes, which is `advance`'s contract") {
    val store = new MemoryStore
    val w = new Worker[String, String, String, P, Async](store.topic("naps"), "nap/1", Timers.over(store),
      (q, _) => Async { if (q == "what?") throw new RuntimeException("down") else "ok" })(twoStep)

    val _ = drive(w.start("n-1"))
    intercept[RuntimeException](drive(w.tick(61000L)))
  }
}
