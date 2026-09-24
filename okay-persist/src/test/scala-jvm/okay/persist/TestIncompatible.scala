package okay.persist

import munit.FunSuite
import okay.{!, +, Async, CanBlock, Delim, Pure, Wf}
import okay.given_CanBlock
import okay.given_Scheduler
import okay.Direct.*
import okay.codec.Schema
import scala.language.implicitConversions

/**
 * A PROGRAM THAT CANNOT READ ITS OWN HISTORY (worker-incompatible,
 * 2026-09-17).
 *
 * `Dialogue` says plainly what it does NOT catch: a program that
 * throws while replaying a journal it ACCEPTED. That is right for one
 * caller of one run — the exception is theirs and reaches them where
 * they ran it.
 *
 * Under a worker it needs a name, because `Failed` would be a lie.
 * `Failed` means "not now": nothing was journalled, the run stands,
 * the next pass asks again. But replay is deterministic by the
 * `Replayable` discipline, so a program that throws on its own
 * accepted history will throw on EVERY pass, for ever. Telling an
 * operator "we will retry" about that is worse than saying nothing.
 *
 * The two are separable without guessing: this one throws while the
 * place is being REBUILT, before any question is asked.
 */
class TestIncompatible extends FunSuite {

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_000L, id = "id", dice = 0.5)

  def drive[A](p: A ! Pure + Async)(using CanBlock): A =
    !.run(Async.run[A, Pure](p))

  /** v1, which wrote the journal */
  def v1(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
    direct:
      val a = !w.pause("1?")
      !w.sleep(60_000L)          // so the run STOPS with history behind it
      val b = !w.pause("2?")
      s"$a/$b"

  /** the same program NAME, new code — and it cannot read what v1
   * wrote. The fold accepts the record (the name matches); the body
   * is what refuses it. */
  def v1prime(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
    direct:
      val a = !w.pause("1?")
      if a == "old" then throw new IllegalStateException("cannot read v1 data")
      !w.sleep(60_000L)
      val b = !w.pause("2?")
      s"$a/$b"

  test("THE POINT: a program that throws on its accepted history is INCOMPATIBLE, not Failed") {
    val store = MemoryStore()
    val t = store.topic("runs")
    val old = Worker[String, String, String, Pure, Async](
      t, "job/1", Timers.over(store), _ => okay.async("old"))(v1)
    // one answer in, asleep, with history behind it
    assertEquals(drive(old.start("r-1")), Worker.Progress.Sleeping(61_000L))
    val before = old.dialogue("r-1").journal
    assertEquals(before.count(_.isRight), 1)

    // the deploy lands: same program name, code that cannot read it
    val fresh = Worker[String, String, String, Pure, Async](
      t, "job/1", Timers.over(store), _ => okay.async("new"),
      isolate = Some(Worker.isolating))(v1prime)

    drive(fresh.advance("r-1")) match
      case Worker.Progress.Incompatible(why) =>
        assert(why.contains("cannot read v1 data"), why)
      case other => fail(s"a bad deploy was reported as $other")

    // and NOTHING was written: the run is exactly where v1 left it
    assertEquals(fresh.dialogue("r-1").journal, before)
  }

  test("an ACTIVITY that fails is still Failed: the two are not merged") {
    val store = MemoryStore()
    val w = Worker[String, String, String, Pure, Async](
      store.topic("runs"), "job/1", Timers.over(store),
      _ => okay.async { throw new RuntimeException("the service is down") },
      isolate = Some(Worker.isolating))(v1)

    // it throws while ANSWERING, not while rebuilding the place
    val _ = intercept[RuntimeException](drive(w.advance("r-1")))
  }

  test("without isolation it still throws, because catching needs the row") {
    val store = MemoryStore()
    val t = store.topic("runs")
    val old = Worker[String, String, String, Pure, Async](
      t, "job/1", Timers.over(store), _ => okay.async("old"))(v1)
    val _ = drive(old.start("r-1"))

    val fresh = Worker[String, String, String, Pure, Async](
      t, "job/1", Timers.over(store), _ => okay.async("new"))(v1prime)
    val _ = intercept[IllegalStateException](drive(fresh.advance("r-1")))
  }

  /** it bounds its own history — and cannot read the seed it wrote */
  def cycle(using w: Wf.Asks[String, String, Wf.Next[String, String], Pure])
      : Wf.Next[String, String] ! Delim + Pure =
    direct:
      val input = !w.pause("input")
      if input == "seed!" then
        throw new IllegalStateException("cannot read my own seed")
      Wf.Next.Continue("seed!")

  test("a chapter boundary rebuilds a place too, and gets the same verdict") {
    val store = MemoryStore()
    val w = Worker[String, String, Wf.Next[String, String], Pure, Async](
      store.topic("runs"), "cycle/1", Timers.over(store),
      _ => okay.async("first"),
      seedOf = Wf.Next.seed, isolate = Some(Worker.isolating))(cycle)

    // the first chapter ends with a seed; the SECOND cannot read it.
    // That is the cold path's failure at a different door, and before
    // this lane it escaped the worker instead of being named.
    drive(w.start("c-1")) match
      case Worker.Progress.Incompatible(why) =>
        assert(why.contains("cannot read my own seed"), why)
      case other => fail(s"a chapter that cannot replay its seed was reported as $other")
  }

  test("the WAKE path rebuilds a place too: a due run gets the same verdict") {
    val store = MemoryStore()
    val t = store.topic("runs")
    val timers = Timers.over(store)
    val old = Worker[String, String, String, Pure, Async](
      t, "job/1", timers, _ => okay.async("old"))(v1)
    assertEquals(drive(old.start("r-1")), Worker.Progress.Sleeping(61_000L))

    // the deploy lands while the run is asleep, so the next thing to
    // touch it is `tick` -> `wake`, which rebuilds the place before it
    // decides whether the timer is real
    val fresh = Worker[String, String, String, Pure, Async](
      t, "job/1", timers, _ => okay.async("new"),
      isolate = Some(Worker.isolating))(v1prime)

    drive(fresh.tick(61_000L)).map(_._2) match
      case List(Worker.Progress.Incompatible(why)) =>
        assert(why.contains("cannot read v1 data"), why)
      case other =>
        fail(s"the wake path reported a bad deploy as $other")
  }
}
