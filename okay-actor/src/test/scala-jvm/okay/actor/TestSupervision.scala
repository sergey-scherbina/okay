package okay.actor

import okay.*
import okay.given

/**
 * The laws of supervision — stage 1, and the part of this module that
 * composition did NOT already give.
 */
class TestSupervision extends munit.FunSuite {

  given Scheduler = Schedulers.loom

  private def await(cond: => Boolean, why: String): Unit =
    val deadline = System.currentTimeMillis() + 5000
    while !cond && System.currentTimeMillis() < deadline do Thread.`yield`()
    assert(cond, why)

  /** LAW 5a: Resume keeps the state, and the failed message is gone */
  test("law: Resume keeps the state and drops the message that threw") {
    val seen = java.util.concurrent.ConcurrentLinkedQueue[Int]()
    val a = Actor.spawn(0, Channel[Int](64), Supervise.Resume) { (n: Int, m: Int) =>
      async {
        if m < 0 then throw RuntimeException("bad")
        val next = n + m
        seen.add(next): Unit
        next
      }
    }.runWith
    val _ = a.tell(1).runWith
    a.tell(-1).runWith: Unit   // throws; state must survive
    val _ = a.tell(1).runWith
    await(seen.size == 2, s"expected two survivors, saw ${seen.size}")
    val out = scala.jdk.CollectionConverters.CollectionHasAsScala(seen).asScala.toList
    assertEquals(out, List(1, 2), "Resume keeps the state the failure left")
    a.stop().runWith
  }

  /** LAW 5b: Restart replaces the state — and it is `fresh()`, called
   * again, not the original value reused, because a restart that
   * shared a mutable initial state would restart into the wreckage */
  test("law: Restart starts from a fresh state, and drops the message") {
    val seen = java.util.concurrent.ConcurrentLinkedQueue[Int]()
    val builds = java.util.concurrent.atomic.AtomicInteger(0)
    val a = Actor.spawn(0, Channel[Int](64),
      Supervise.Restart(() => { builds.incrementAndGet(); 0 })) { (n: Int, m: Int) =>
      async {
        if m < 0 then throw RuntimeException("bad")
        val next = n + m
        seen.add(next): Unit
        next
      }
    }.runWith
    val _ = a.tell(5).runWith
    a.tell(-1).runWith: Unit   // throws; state resets to 0
    val _ = a.tell(3).runWith
    await(seen.size == 2, s"expected two survivors, saw ${seen.size}")
    val out = scala.jdk.CollectionConverters.CollectionHasAsScala(seen).asScala.toList
    assertEquals(out, List(5, 3), "after a restart the count begins again")
    assertEquals(builds.get, 1, "fresh() is called once, per restart")
    a.stop().runWith
  }

  /** LAW 6: Escalate hands the throwable on EXACTLY once and stops */
  test("law: Escalate reports once and stops the actor") {
    val reported = java.util.concurrent.ConcurrentLinkedQueue[String]()
    val a = Actor.spawn(0, Channel[Int](64),
      Supervise.Escalate(e => reported.add(e.getMessage): Unit)) { (n: Int, m: Int) =>
      async { if m < 0 then throw RuntimeException("boom") else n + m }
    }.runWith
    val _ = a.tell(-1).runWith
    await(reported.size == 1, s"expected one report, saw ${reported.size}")
    assertEquals(scala.jdk.CollectionConverters.CollectionHasAsScala(reported).asScala.toList,
      List("boom"))
    await(a.stopped, "Escalate must stop the child")
    assertEquals(a.tell(1).runWith, false, "and it stays stopped")
  }

  /** the default, stated in the spec as a decision: an actor that
   * fails and quietly carries on is how a system goes wrong silently */
  test("law: the default is Stop — a failure ends the actor") {
    val a = Actor.spawn(0) { (n: Int, m: Int) =>
      async { if m < 0 then throw RuntimeException("boom") else n + m }
    }.runWith
    val _ = a.tell(-1).runWith
    await(a.stopped, "an unsupervised failure must stop the actor")
  }

  /** the decision that costs the most to get wrong: redelivery is how
   * a system loops for ever on one poisonous message */
  test("law: a message that threw is never retried") {
    val attempts = java.util.concurrent.atomic.AtomicInteger(0)
    val a = Actor.spawn(0, Channel[Int](64), Supervise.Resume) { (n: Int, m: Int) =>
      async {
        if m < 0 then { attempts.incrementAndGet(); throw RuntimeException("bad") }
        n + m
      }
    }.runWith
    val _ = a.tell(-1).runWith
    val _ = a.tell(1).runWith
    Thread.sleep(200)
    assertEquals(attempts.get, 1, "the poisonous message must be seen once, not for ever")
    a.stop().runWith
  }
}
