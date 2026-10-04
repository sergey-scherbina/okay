package okay2.persist

import munit.FunSuite
import okay2.{!, Wf, pure}
import okay2.async.Async
import okay2.platform._

/**
 * A PROGRAM THAT CANNOT READ ITS OWN HISTORY (okay-persist's
 * TestIncompatible; worker-incompatible): under a worker it needs a
 * name, because `Failed` ("not now") would be a lie — replay is
 * deterministic, so a program that throws on its accepted history throws
 * on EVERY pass. It throws while the place is being REBUILT, before any
 * question is asked, and that is how the two are told apart.
 */
class TestIncompatible extends FunSuite {
  import DialogueFixtures._
  import WorkflowFixtures._
  import WorkerFixtures._

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1000L, id = "id", dice = 0.5)

  /** v1, which wrote the journal */
  def v1(w: W): String ! Rw = for {
    a <- w.pause("1?")
    _ <- w.sleep(60000L)          // so the run STOPS with history behind it
    b <- w.pause("2?")
  } yield s"$a/$b"

  /** the same program NAME, new code — and it cannot read what v1 wrote */
  def v1prime(w: W): String ! Rw = for {
    a <- w.pause("1?")
    _ = if (a == "old") throw new IllegalStateException("cannot read v1 data")
    _ <- w.sleep(60000L)
    b <- w.pause("2?")
  } yield s"$a/$b"

  def job(store: MemoryStore, t: Topic, answer: String, timers: Option[Timers] = None, iso: Boolean = false)(body: W => String ! Rw): Worker[String, String, String, P, Async] =
    new Worker[String, String, String, P, Async](t, "job/1", timers.getOrElse(Timers.over(store)), say(answer),
      isolate = if (iso) Some(Worker.isolating) else None)(body)

  test("THE POINT: a program that throws on its accepted history is INCOMPATIBLE, not Failed") {
    val store = new MemoryStore
    val t = store.topic("runs")
    val old = job(store, t, "old")(v1)
    assertEquals(drive(old.start("r-1")), Worker.Progress.Sleeping(61000L): Worker.Progress[String])
    val before = old.dialogue("r-1").journal
    assertEquals(before.count(_.isRight), 1)

    // the deploy: the same name, a body that cannot read the old answer
    val fresh = job(store, t, "new", iso = true)(v1prime)
    drive(fresh.advance("r-1")) match {
      case Worker.Progress.Incompatible(why) => assert(why.contains("cannot read v1 data"), why)
      case other => fail(s"a bad deploy was reported as $other")
    }

    // and nothing was written: the history is intact for the fix
    assertEquals(fresh.dialogue("r-1").journal, before)
  }

  test("an ACTIVITY that fails is still Failed: the two are not merged") {
    val store = new MemoryStore
    val w = new Worker[String, String, String, P, Async](store.topic("runs"), "job/1", Timers.over(store),
      (_, _) => Async[String](throw new RuntimeException("the service is down")),
      isolate = Some(Worker.isolating))(v1)
    // `advance` keeps its contract: one run, its caller's exception
    intercept[RuntimeException](drive(w.advance("r-1")))
  }

  test("without isolation it still throws, because catching needs the row") {
    val store = new MemoryStore
    val t = store.topic("runs")
    val _ = drive(job(store, t, "old")(v1).start("r-1"))
    intercept[IllegalStateException](drive(job(store, t, "new")(v1prime).advance("r-1")))
  }

  /** it bounds its own history — and cannot read the seed it wrote */
  def cycle(w: Wf.Asks[String, String, Wf.Next[String, String], P]): Wf.Next[String, String] ! Rw =
    w.pause("input").flatMap { input =>
      if (input == "seed!") throw new IllegalStateException("cannot read my own seed")
      pure[Rw, Wf.Next[String, String]](Wf.Next.Continue("seed!"))
    }

  test("a chapter boundary rebuilds a place too, and gets the same verdict") {
    val store = new MemoryStore
    val w = new Worker[String, String, Wf.Next[String, String], P, Async](store.topic("runs"), "cycle/1", Timers.over(store),
      say("first"), seedOf = (n: Wf.Next[String, String]) => Wf.Next.seed(n), isolate = Some(Worker.isolating))(cycle)

    drive(w.start("c-1")) match {
      case Worker.Progress.Incompatible(why) => assert(why.contains("cannot read my own seed"), why)
      case other => fail(s"a chapter that cannot replay its seed was reported as $other")
    }
  }

  test("the WAKE path rebuilds a place too: a due run gets the same verdict") {
    val store = new MemoryStore
    val t = store.topic("runs")
    val timers = Timers.over(store)
    assertEquals(drive(job(store, t, "old", Some(timers))(v1).start("r-1")), Worker.Progress.Sleeping(61000L): Worker.Progress[String])

    val fresh = job(store, t, "new", Some(timers), iso = true)(v1prime)
    drive(fresh.tick(61000L)).map(_._2) match {
      case List(Worker.Progress.Incompatible(why)) => assert(why.contains("cannot read v1 data"), why)
      case other => fail(s"the wake path reported a bad deploy as $other")
    }
  }
}
