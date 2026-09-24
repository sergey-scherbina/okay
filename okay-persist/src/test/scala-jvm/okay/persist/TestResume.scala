package okay.persist

import munit.FunSuite
import okay.{!, +, Async, CanBlock, Delim, Pure, Wf}
import okay.given_CanBlock
import okay.Direct.*
import okay.codec.Schema
import scala.language.implicitConversions

/**
 * KEEPING THE PROGRAM BETWEEN CALLS (dialogue-resume-cache,
 * 2026-09-17).
 *
 * The first test MEASURES the claim rather than asserting that the
 * feature exists: a counter in the program body counts how many times
 * the body is BUILT, which is how many times the journal was
 * replayed. A test that only checked the answers would pass against a
 * cache that never hit.
 *
 * The case it measures is the one a worker loop actually spends its
 * time on: touching runs that are still waiting. Without a cache each
 * touch replays the whole journal; with one it costs an offset read.
 */
class TestResume extends FunSuite {

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_000L, id = "id", dice = 0.5)

  def drive[A](p: A ! Pure + Async)(using CanBlock): A =
    !.run(Async.run[A, Pure](p))

  /** counts its own builds: one per replay */
  class Counted:
    var builds = 0
    def body(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
      direct:
        builds += 1
        val who = !w.pause("who?")
        val ok = !w.awaitSignal("go")
        s"$who/$ok"

  def worker(store: MemoryStore, t: Topic, c: Counted,
             cache: Option[Resume[Wf.Ask[String], Wf.Ans[String], String, Pure]],
             sigs: Option[Signals] = None) =
    Worker[String, String, String, Pure, Async](t, "waiting/1", Timers.over(store),
      _ => okay.async("ada"), signals = sigs, resume = cache)(c.body)

  test("THE POINT: five touches of a waiting run replay ONCE, not five times") {
    val store = MemoryStore()
    val t = store.topic("waits")
    val cold = Counted()
    val warm = Counted()
    val cache = Resume[Wf.Ask[String], Wf.Ans[String], String, Pure]()

    val a = worker(store, t, cold, None)
    val b = worker(MemoryStore(), store.topic("waits2"), warm, Some(cache))

    // both start, then are touched four more times while waiting
    val _ = drive(a.start("w-1"))
    val _ = drive(b.start("w-1"))
    for _ <- 1 to 4 do
      val _ = drive(a.advance("w-1"))
      val _ = drive(b.advance("w-1"))

    assertEquals(cold.builds, 5, "the uncached worker did not replay per call")
    assertEquals(warm.builds, 1,
      s"the cached worker replayed ${warm.builds} times instead of once")
  }

  test("somebody else writes: the cache notices and replays") {
    val store = MemoryStore()
    val t = store.topic("waits")
    val c = Counted()
    val cache = Resume[Wf.Ask[String], Wf.Ans[String], String, Pure]()
    val sigs = Signals.over(store)
    val w = worker(store, t, c, Some(cache), Some(sigs))

    assertEquals(drive(w.start("w-1")), Worker.Progress.Waiting(Wf.Wait.Signal("go")))
    assertEquals(c.builds, 1)

    // the signal arrives and is delivered into the journal, which
    // moves the log under the cached program
    val _ = sigs.send("w-1", "go", "yes")
    assertEquals(drive(w.advance("w-1")), Worker.Progress.Finished("ada/yes"))
    assert(c.builds > 1, "the cached program was driven over a journal that had moved")
  }

  test("the cache never changes the answer, only how often it is derived") {
    val store = MemoryStore()
    val sigs = Signals.over(store)
    def run(cache: Boolean): String =
      val s = MemoryStore()
      val sg = Signals.over(s)
      val c = Counted()
      val w = worker(s, s.topic("waits"), c,
        Option.when(cache)(Resume[Wf.Ask[String], Wf.Ans[String], String, Pure]()),
        Some(sg))
      val _ = drive(w.start("w-1"))
      val _ = sg.send("w-1", "go", "yes")
      drive(w.advance("w-1")) match
        case Worker.Progress.Finished(r) => r
        case other => fail(s"did not finish: $other")
    assertEquals(run(cache = false), run(cache = true))
    val _ = sigs
  }

  test("a finished run is dropped: the cache does not hold programs alive") {
    val store = MemoryStore()
    val sigs = Signals.over(store)
    val cache = Resume[Wf.Ask[String], Wf.Ans[String], String, Pure]()
    val w = worker(store, store.topic("waits"), Counted(), Some(cache), Some(sigs))

    val _ = drive(w.start("w-1"))
    assertEquals(cache.ids, List("w-1"), "a waiting run was not kept")
    val _ = sigs.send("w-1", "go", "yes")
    val _ = drive(w.advance("w-1"))
    assertEquals(cache.ids, Nil, "a finished run is still held")
  }

  test("the bound holds: the least recently used goes first") {
    val store = MemoryStore()
    val t = store.topic("waits")
    val cache = Resume[Wf.Ask[String], Wf.Ans[String], String, Pure](max = 2)
    val w = worker(store, t, Counted(), Some(cache))

    val _ = drive(w.start("a"))
    val _ = drive(w.start("b"))
    assertEquals(cache.ids, List("a", "b"))

    // touching `a` makes it the most recent, so `b` is the one to go
    val _ = drive(w.advance("a"))
    val _ = drive(w.start("c"))
    assertEquals(cache.ids, List("a", "c"), s"evicted the wrong one: ${cache.ids}")
    assertEquals(cache.size, 2)
  }
}
