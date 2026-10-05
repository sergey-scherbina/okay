package okay2.persist

import munit.FunSuite
import okay2.workflow.Wf
import okay2.async.Async

/**
 * AN ADVISORY LEASE (okay-persist's TestLease; workflow-lease). The
 * fourth test must never be deleted: it forces the race the lease cannot
 * prevent — two workers both believing they hold it — and asserts that
 * the JOURNAL is still one. `expect` is the guard; this is only the
 * optimisation.
 */
class TestLease extends FunSuite {
  import DialogueFixtures._
  import WorkflowFixtures._
  import WorkerFixtures._

  implicit val rt: Wf.Runtime = Wf.Runtime.scripted(millis = 1000L, id = "id", dice = 0.5)

  /** a worker with a lease and a clock the test drives */
  def leased(store: MemoryStore, t: Topic, ls: Option[Leases], who: String, now: () => Long, answers: String = "ada"): Worker[String, String, String, P, Async] =
    new Worker[String, String, String, P, Async](t, "nap/1", Timers.over(store), say(answers), leases = ls, owner = who,
      leaseMillis = 1000L, clock = now)(nap)

  val sleeping: Worker.Progress[String] = Worker.Progress.Sleeping(61000L)

  test("a worker takes the lease, works, and gives it back") {
    val store = new MemoryStore
    val ls = Leases.over(store)
    val now = 100L
    val w = leased(store, store.topic("naps"), Some(ls), "w1", () => now)

    assertEquals(drive(w.start("n-1")), sleeping)
    assertEquals(ls.held("n-1", now), None, "the lease was kept over a sleeping run")
  }

  test("a second worker finds it held and drives NOTHING") {
    val store = new MemoryStore
    val ls = Leases.over(store)
    val now = 100L
    val t = store.topic("naps")
    val a = leased(store, t, Some(ls), "w1", () => now)
    val b = leased(store, t, Some(ls), "w2", () => now, answers = "bob")

    assert(ls.acquire("n-1", "w1", now + 1000L, now))
    assertEquals(drive(b.advance("n-1")), Worker.Progress.Busy("w1"): Worker.Progress[String])
    assertEquals(b.dialogue("n-1").journal, Nil, "the busy worker drove anyway")

    assert(ls.release("n-1", "w1", now))
    assertEquals(drive(a.start("n-1")), sleeping)
  }

  test("an EXPIRED lease is nobody's") {
    val ls = Leases.over(new MemoryStore)
    assert(ls.acquire("n-1", "w1", untilMillis = 500L, nowMillis = 100L))
    assertEquals(ls.held("n-1", 400L).map(_.owner), Some("w1"))
    assertEquals(ls.held("n-1", 600L), None, "an expired lease still reads as held")
    assert(ls.acquire("n-1", "w2", untilMillis = 1600L, nowMillis = 600L))
    assertEquals(ls.held("n-1", 700L).map(_.owner), Some("w2"))
  }

  test("THE POINT: an expired lease does not FENCE, and the journal is still one") {
    val store = new MemoryStore
    val ls = Leases.over(store)
    val t = store.topic("naps")

    assert(ls.acquire("n-1", "w1", untilMillis = 1000L, nowMillis = 100L))
    // w1 stalls past its lease; w2 takes the expired one
    assert(ls.acquire("n-1", "w2", untilMillis = 3000L, nowMillis = 1100L))
    assertEquals(ls.held("n-1", 1100L).map(_.owner), Some("w2"))

    // both workers drive anyway, as two that believe they hold it would
    val a = leased(store, t, None, "w1", () => 1100L, answers = "ada")
    val b = leased(store, t, None, "w2", () => 1100L, answers = "bob")
    assertEquals(drive(a.start("n-1")), sleeping)
    assertEquals(drive(b.start("n-1")), sleeping)

    val j = a.dialogue("n-1").journal
    assertEquals(j.count(_.isRight), 1, s"the question was answered twice: $j")
    assertEquals(j.head, Right("ada"))
  }

  /** `acquire` reads, decides and writes, so two workers whose reads both
   * land before either write will both hold it. Sequential calls cannot
   * interleave; conceded in `Leases`'s header, stated here */
  test("read-then-write is not atomic: stated, not staged") {
    val ls = Leases.over(new MemoryStore)
    assert(ls.acquire("n-1", "w1", 1000L, 100L))
    assert(!ls.acquire("n-1", "w2", 1000L, 100L), "SEQUENTIAL acquisition should exclude; only a true race does not")
  }

  test("only the holder may release or renew") {
    val ls = Leases.over(new MemoryStore)
    assert(ls.acquire("n-1", "w1", 1100L, 100L))
    assert(!ls.release("n-1", "w2", 200L), "a stranger freed somebody else's work")
    assert(!ls.renew("n-1", "w2", 5000L, 200L))
    assertEquals(ls.held("n-1", 200L).map(_.owner), Some("w1"))

    assert(ls.renew("n-1", "w1", 9000L, 200L))
    assertEquals(ls.held("n-1", 5000L).map(_.owner), Some("w1"))
    assert(ls.release("n-1", "w1", 5100L))
    assertEquals(ls.held("n-1", 5200L), None)
  }

  test("standing: what an operator sees, expired ones excluded") {
    val ls = Leases.over(new MemoryStore)
    assert(ls.acquire("a", "w1", 1000L, 100L))
    assert(ls.acquire("b", "w2", 5000L, 100L))
    assertEquals(ls.standing(200L).map { case (k, v) => k -> v.owner }, Map("a" -> "w1", "b" -> "w2"))
    assertEquals(ls.standing(2000L).map { case (k, v) => k -> v.owner }, Map("b" -> "w2"))
  }

  test("no leases at all: every worker believes it is free, which is where we started") {
    val store = new MemoryStore
    val w = leased(store, store.topic("naps"), None, "w1", () => 100L)
    assertEquals(drive(w.start("n-1")), sleeping)
  }
}
