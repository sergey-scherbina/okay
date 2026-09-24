package okay.persist

import munit.FunSuite
import okay.{!, +, Async, CanBlock, Delim, Pure, Wf}
import okay.given_CanBlock
import okay.Direct.*
import okay.codec.Schema
import scala.language.implicitConversions

/**
 * AN ADVISORY LEASE (workflow-lease, 2026-09-17).
 *
 * The fourth test is the one that must never be deleted: it forces
 * the race the lease cannot prevent — two workers both believing they
 * hold it — and asserts that the JOURNAL is still one. That is the
 * standing reminder that `expect` is the guard and this is only the
 * optimisation. A test suite that showed the lease excluding, and
 * nothing else, would invite somebody to rely on it.
 */
class TestLease extends FunSuite {

  given Schema[Wf.SysA] = Schema.derived
  given Schema[Wf.Ans[String]] = Schema.derived
  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_000L, id = "id", dice = 0.5)

  def drive[A](p: A ! Pure + Async)(using CanBlock): A =
    !.run(Async.run[A, Pure](p))

  def nap(using w: Wf.Asks[String, String, String, Pure]): String ! Delim + Pure =
    direct:
      val who = !w.pause("who?")
      !w.sleep(60_000L)
      s"$who woke"

  /** a worker with a lease and a clock the test drives */
  def worker(store: MemoryStore, t: Topic, ls: Option[Leases], who: String,
             now: () => Long, answers: String = "ada") =
    Worker[String, String, String, Pure, Async](t, "nap/1", Timers.over(store),
      _ => okay.async(answers), leases = ls, owner = who,
      leaseMillis = 1_000L, clock = now)(nap)

  test("a worker takes the lease, works, and gives it back") {
    val store = MemoryStore()
    val ls = Leases.over(store)
    val now = 100L
    val w = worker(store, store.topic("naps"), Some(ls), "w1", () => now)

    assertEquals(drive(w.start("n-1")), Worker.Progress.Sleeping(61_000L))
    // a run that is sleeping is not being worked on
    assertEquals(ls.held("n-1", now), None, "the lease was kept over a sleeping run")
  }

  test("a second worker finds it held and drives NOTHING") {
    val store = MemoryStore()
    val ls = Leases.over(store)
    val now = 100L
    val t = store.topic("naps")
    val a = worker(store, t, Some(ls), "w1", () => now)
    val b = worker(store, t, Some(ls), "w2", () => now, answers = "bob")

    // w1 takes it by hand and holds it, as it would while working
    assert(ls.acquire("n-1", "w1", now + 1_000L, now))
    assertEquals(drive(b.advance("n-1")), Worker.Progress.Busy("w1"))
    assertEquals(b.dialogue("n-1").journal, Nil, "the busy worker drove anyway")

    // and when it is free, the same call works
    assert(ls.release("n-1", "w1", now))
    assertEquals(drive(a.start("n-1")), Worker.Progress.Sleeping(61_000L))
  }

  test("an EXPIRED lease is nobody's") {
    val store = MemoryStore()
    val ls = Leases.over(store)
    assert(ls.acquire("n-1", "w1", untilMillis = 500L, nowMillis = 100L))
    assertEquals(ls.held("n-1", 400L).map(_.owner), Some("w1"))
    assertEquals(ls.held("n-1", 600L), None, "an expired lease still reads as held")
    assert(ls.acquire("n-1", "w2", untilMillis = 1_600L, nowMillis = 600L))
    assertEquals(ls.held("n-1", 700L).map(_.owner), Some("w2"))
  }

  test("THE POINT: an expired lease does not FENCE, and the journal is still one") {
    val store = MemoryStore()
    val ls = Leases.over(store)
    val t = store.topic("naps")

    // w1 takes the lease and starts working. Everything here is
    // correct and nobody is at fault for what follows.
    assert(ls.acquire("n-1", "w1", untilMillis = 1_000L, nowMillis = 100L))

    // its lease expires while it is still inside a slow call. w2 now
    // takes the lease LEGITIMATELY -- that is what expiry is for --
    // and both of them are working on the same run. No lease scheme
    // of this shape can prevent it: expiry is decided by a clock, and
    // a clock cannot stop a thread. Closing it needs a fencing token
    // checked AT THE WRITE, which is exactly what `expect` already is.
    assert(ls.acquire("n-1", "w2", untilMillis = 3_000L, nowMillis = 1_100L))
    assertEquals(ls.held("n-1", 1_100L).map(_.owner), Some("w2"))

    // ...so both drive, from the same standing start
    val a = worker(store, t, None, "w1", () => 1_100L, answers = "ada")
    val b = worker(store, t, None, "w2", () => 1_100L, answers = "bob")
    assertEquals(drive(a.start("n-1")), Worker.Progress.Sleeping(61_000L))
    assertEquals(drive(b.start("n-1")), Worker.Progress.Sleeping(61_000L))

    // AND THE JOURNAL IS ONE. This is what keeps the engine correct,
    // and it is not the lease.
    val j = a.dialogue("n-1").journal
    assertEquals(j.count(_.isRight), 1, s"the question was answered twice: $j")
    assertEquals(j.head, Right("ada"))
  }

  /**
   * The OTHER hole, which this suite states rather than reproduces:
   * `acquire` reads, decides and writes, so two workers whose reads
   * both land before either write will both come away holding it.
   * Sequential calls cannot interleave — the first test written here
   * tried, and its second `acquire` correctly refused — so the only
   * way to show it is true concurrency, which would make this suite
   * flaky to prove something the design already concedes. It is
   * conceded in `Leases`'s header instead, and the expiry hole above
   * makes the same point deterministically.
   */
  test("read-then-write is not atomic: stated, not staged") {
    val ls = Leases.over(MemoryStore())
    assert(ls.acquire("n-1", "w1", 1_000L, 100L))
    assert(!ls.acquire("n-1", "w2", 1_000L, 100L),
      "SEQUENTIAL acquisition should exclude; only a true race does not")
  }

  test("only the holder may release or renew") {
    val store = MemoryStore()
    val ls = Leases.over(store)
    assert(ls.acquire("n-1", "w1", 1_100L, 100L))
    assert(!ls.release("n-1", "w2", 200L), "a stranger freed somebody else's work")
    assert(!ls.renew("n-1", "w2", 5_000L, 200L))
    assertEquals(ls.held("n-1", 200L).map(_.owner), Some("w1"))

    assert(ls.renew("n-1", "w1", 9_000L, 200L))
    assertEquals(ls.held("n-1", 5_000L).map(_.owner), Some("w1"))
    assert(ls.release("n-1", "w1", 5_100L))
    assertEquals(ls.held("n-1", 5_200L), None)
  }

  test("standing: what an operator sees, expired ones excluded") {
    val store = MemoryStore()
    val ls = Leases.over(store)
    assert(ls.acquire("a", "w1", 1_000L, 100L))
    assert(ls.acquire("b", "w2", 5_000L, 100L))
    assertEquals(ls.standing(200L).view.mapValues(_.owner).toMap, Map("a" -> "w1", "b" -> "w2"))
    assertEquals(ls.standing(2_000L).view.mapValues(_.owner).toMap, Map("b" -> "w2"))
  }

  test("no leases at all: every worker believes it is free, which is where we started") {
    val store = MemoryStore()
    val t = store.topic("naps")
    val w = worker(store, t, None, "w1", () => 100L)
    assertEquals(drive(w.start("n-1")), Worker.Progress.Sleeping(61_000L))
  }
}
