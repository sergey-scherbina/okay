package okay.actor

import okay.*
import okay.given

/**
 * The laws of specs/actor.md, stage 0 — written as laws because the
 * guarantees they state are the reason to use an actor at all. "One
 * message at a time" is not an implementation note: it is what lets
 * the state be a plain value with no synchronisation anywhere.
 */
class TestActorLaws extends munit.FunSuite {

  given Scheduler = Schedulers.loom

  /** LAW 1: one at a time — behaviours never overlap, so nothing is
   * lost even when many senders push at once. A counter is the
   * sharpest form: with overlap it would lose increments. */
  test("law: one message at a time, so no update is lost") {
    val done = java.util.concurrent.CountDownLatch(1)
    val seen = java.util.concurrent.atomic.AtomicInteger(0)
    val a = Actor.spawn(0) { (n: Int, m: Int) =>
      async {
        val next = n + m
        seen.set(next)
        if next == 4000 then done.countDown()
        next
      }
    }.runWith
    val ps = (0 until 8).map(_ => Thread.ofVirtual().start { () =>
      (0 until 500).foreach(_ => { val _ = a.tell(1).runWith })
    })
    ps.foreach(_.join())
    assert(done.await(10, java.util.concurrent.TimeUnit.SECONDS),
      s"expected 4000 increments, the actor saw ${seen.get}")
    a.stop().runWith
  }

  /** LAW 2: order per sender — inherited from the channel, and
   * checked here because an actor is where a caller relies on it */
  test("law: one sender's messages arrive in the order it sent them") {
    val got = java.util.concurrent.ConcurrentLinkedQueue[Int]()
    val done = java.util.concurrent.CountDownLatch(1)
    val a = Actor.spawn(()) { (_: Unit, m: Int) =>
      async { got.add(m): Unit; if m == 499 then done.countDown() }
    }.runWith
    (0 until 500).foreach(i => { val _ = a.tell(i).runWith })
    assert(done.await(10, java.util.concurrent.TimeUnit.SECONDS))
    val out = scala.jdk.CollectionConverters.CollectionHasAsScala(got).asScala.toList
    assertEquals(out, out.sorted, "a sender's own order must be kept")
    a.stop().runWith
  }

  /** LAW 3: stop drains — the strong contract, carried up. Every
   * message already accepted is handled before the actor ends. */
  test("law: stop drains what was already accepted") {
    val handled = java.util.concurrent.atomic.AtomicInteger(0)
    val mailbox = Channel[Int](1024)
    val a = Actor.spawn(0, mailbox, Supervise.Stop) { (n: Int, _: Int) =>
      async { handled.incrementAndGet(): Unit; n + 1 }
    }.runWith
    (0 until 200).foreach(i => { val _ = a.tell(i).runWith })
    a.stop().runWith
    val deadline = System.currentTimeMillis() + 5000
    while handled.get < 200 && System.currentTimeMillis() < deadline do Thread.`yield`()
    assertEquals(handled.get, 200, "a drain-on-close mailbox must not drop accepted work")
  }

  /** LAW 4: a stopped actor accepts nothing — and says so with a
   * `false` rather than an exception, because a producer outliving
   * its actor is ordinary */
  test("law: a stopped actor answers false, for ever, and never throws") {
    val a = Actor.spawn(0) { (n: Int, m: Int) => async(n + m) }.runWith
    assert(a.tell(1).runWith, "an running actor accepts")
    a.stop().runWith
    val deadline = System.currentTimeMillis() + 5000
    while !a.stopped && System.currentTimeMillis() < deadline do Thread.`yield`()
    assertEquals(a.tell(2).runWith, false)
    assertEquals(a.tell(3).runWith, false, "and it stays false")
  }
}
