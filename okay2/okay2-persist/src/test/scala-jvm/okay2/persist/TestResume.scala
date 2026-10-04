package okay2.persist

import munit.FunSuite
import okay2.{!, Wf}
import okay2.async.Async

/**
 * KEEPING THE PROGRAM BETWEEN CALLS (okay-persist's TestResume;
 * dialogue-resume-cache). The first test MEASURES the claim: a counter
 * in the program body counts how many times the body is BUILT, which is
 * how many times the journal was replayed.
 */
class TestResume extends FunSuite {
  import DialogueFixtures._
  import WorkflowFixtures._
  import WorkerFixtures._

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1000L, id = "id", dice = 0.5)

  type Cache = Resume[Wf.Ask[String], Wf.Ans[String], String, P]

  /** counts its own builds: one per replay */
  class Counted {
    var builds = 0
    def body(w: W): String ! Rw = {
      builds += 1
      for { who <- w.pause("who?"); ok <- w.awaitSignal("go") } yield s"$who/$ok"
    }
  }

  def worker(store: MemoryStore, t: Topic, c: Counted, cache: Option[Cache], sigs: Option[Signals] = None): Worker[String, String, String, P, Async] =
    new Worker[String, String, String, P, Async](t, "waiting/1", Timers.over(store), say("ada"), signals = sigs, resume = cache)(c.body)

  test("THE POINT: five touches of a waiting run replay ONCE, not five times") {
    val store = new MemoryStore
    val cold = new Counted
    val warm = new Counted
    val cache = new Cache()

    val a = worker(store, store.topic("waits"), cold, None)
    val b = worker(new MemoryStore, store.topic("waits2"), warm, Some(cache))

    // both start, then are touched four more times while waiting
    val _ = drive(a.start("w-1"))
    val _ = drive(b.start("w-1"))
    for (_ <- 1 to 4) {
      val _ = drive(a.advance("w-1"))
      val _ = drive(b.advance("w-1"))
    }

    assertEquals(cold.builds, 5, "the uncached worker did not replay per call")
    assertEquals(warm.builds, 1, s"the cached worker replayed ${warm.builds} times instead of once")
  }

  test("somebody else writes: the cache notices and replays") {
    val store = new MemoryStore
    val c = new Counted
    val sigs = Signals.over(store)
    val w = worker(store, store.topic("waits"), c, Some(new Cache()), Some(sigs))

    assertEquals(drive(w.start("w-1")), Worker.Progress.Waiting(Wf.Wait.Signal("go")): Worker.Progress[String])
    assertEquals(c.builds, 1)

    // the signal is delivered into the journal, which moves the log
    // under the cached program
    val _ = sigs.send("w-1", "go", "yes")
    assertEquals(drive(w.advance("w-1")), Worker.Progress.Finished("ada/yes"): Worker.Progress[String])
    assert(c.builds > 1, "the cached program was driven over a journal that had moved")
  }

  test("the cache never changes the answer, only how often it is derived") {
    def run(cache: Boolean): String = {
      val s = new MemoryStore
      val sg = Signals.over(s)
      val w = worker(s, s.topic("waits"), new Counted, if (cache) Some(new Cache()) else None, Some(sg))
      val _ = drive(w.start("w-1"))
      val _ = sg.send("w-1", "go", "yes")
      drive(w.advance("w-1")) match {
        case Worker.Progress.Finished(r) => r
        case other => fail(s"did not finish: $other")
      }
    }
    assertEquals(run(cache = false), run(cache = true))
  }

  test("a finished run is dropped: the cache does not hold programs alive") {
    val store = new MemoryStore
    val sigs = Signals.over(store)
    val cache = new Cache()
    val w = worker(store, store.topic("waits"), new Counted, Some(cache), Some(sigs))

    val _ = drive(w.start("w-1"))
    assertEquals(cache.ids, List("w-1"), "a waiting run was not kept")
    val _ = sigs.send("w-1", "go", "yes")
    val _ = drive(w.advance("w-1"))
    assertEquals(cache.ids, Nil, "a finished run is still held")
  }

  test("the bound holds: the least recently used goes first") {
    val store = new MemoryStore
    val cache = new Cache(max = 2)
    val w = worker(store, store.topic("waits"), new Counted, Some(cache))

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
