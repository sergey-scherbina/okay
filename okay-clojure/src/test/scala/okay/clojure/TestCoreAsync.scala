package okay.clojure

import okay.{Accepted, Stage}
import okay.given
import clojure.lang.IFn
import java.util.concurrent.atomic.AtomicReference

/**
 * `CoreAsyncChannel` beyond the laws (specs/clojure.md, stage 3): real
 * Clojure on the other end — a `go` block producing, `into` consuming, a
 * transducer inside the channel — and the three places the view has to
 * work AROUND core.async: a cancelled receive, a withdrawn send, and a
 * two-phase close. Each of those three is written so its mutant fails
 * it (see the spec's Results).
 */
class TestCoreAsync extends munit.FunSuite {

  def clj(src: String): AnyRef = Clj.eval(src).fold(e => fail(e), identity)
  def fn(ns: String, name: String): IFn = Clj.fn(ns, name).fold(e => fail(e), identity)

  /** drain an okay channel on this thread, to its end */
  def drain[A](c: CoreAsyncChannel[A]): List[A] =
    Iterator.continually(c.receiveBlocking()).takeWhile(_.isDefined).flatten.toList

  // INTEGRATION scope (clojure-go-block-timeout-under-load, 2026-09-25):
  // 200 elements through a 4-slot buffer are up to 200 park/unpark handoffs
  // between core.async's go pool and this thread, and each waits for
  // the OS scheduler. On a saturated shared box (load 80-220 on 14
  // cores) that wall time crossed munit's 30 s in FIVE unrelated gates
  // in one day, and the suite alone was green every time: a budget,
  // not a defect. A longer timeout or a smaller n would weaken the test;
  // the policy is the tag. `sbt integrationTest` still runs it.
  test("a Clojure go block produces, okay consumes, in order, to the end".tag(new munit.Tag("Live"))) {
    clj("(require 'clojure.core.async)")
    clj("""(defn okay-test-produce [ch n]
             (clojure.core.async/go
               (dotimes [i n] (clojure.core.async/>! ch i))
               (clojure.core.async/close! ch)))""")
    val c = CoreAsync.channel[java.lang.Long](4)
    fn("user", "okay-test-produce").invoke(c.chan, Long.box(200L)): Unit
    assertEquals(drain(c).map(_.longValue), (0L until 200L).toList)
  }

  test("okay produces, a Clojure `into` consumes") {
    val c = CoreAsync.channel[java.lang.Long](4)
    val collected = fn("clojure.core.async", "into").invoke(clj("[]"), c.chan)   // a channel of one vector
    val t = Thread.ofVirtual().start { () =>
      (1L to 100L).foreach(i => { val _ = c.sendBlocking(Long.box(i)) })
      c.close()
    }
    val vec = fn("clojure.core.async", "<!!").invoke(collected)
    t.join()
    assertEquals(vec.toString, (1 to 100).mkString("[", " ", "]"))
  }

  test("an okay stage as the channel's own transducer: (chan 10 xf)") {
    val runningSum = Stage.mapAccumulate[Long, Long, Long](0L)((s, i) => (s + i, s + i))
    val ch = fn("clojure.core.async", "chan").invoke(Long.box(10L), Transducers.of(runningSum))
    val c = CoreAsync.of[java.lang.Long](ch)
    (1L to 5L).foreach(i => assert(c.sendBlocking(Long.box(i))))
    c.close()
    assertEquals(drain(c).map(_.longValue), List(1L, 3L, 6L, 10L, 15L))
  }

  test("a cancelled receive loses nothing and lets nothing overtake (the stash)") {
    val c = CoreAsync.channel[java.lang.Long](8)
    val gaveUp: c.End => Unit = _ => fail("a cancelled receiver must not be answered")
    c.receiveAsync(gaveUp)                    // a take! is now in flight for it
    c.cancelReceive(gaveUp)
    val put = fn("clojure.core.async", ">!!")
    put.invoke(c.chan, Long.box(7L)): Unit     // arrives for the receiver that gave up
    put.invoke(c.chan, Long.box(8L)): Unit
    assertEquals(c.receiveBlocking().map(_.longValue), Some(7L))
    assertEquals(c.receiveBlocking().map(_.longValue), Some(8L))
  }

  /** send without blocking, recording the answer */
  def sendRecorded(c: CoreAsyncChannel[java.lang.Long], x: Long, answer: AtomicReference[Option[Boolean]]): Accepted =
    val k: Accepted = accepted => answer.set(Some(accepted))
    c.sendAsync(Long.box(x))(k)
    k

  test("a queued send that is withdrawn is never delivered") {
    val c = CoreAsync.channel[java.lang.Long](1)
    assert(c.offer(Long.box(1L)))                                  // the buffer is full
    val y = AtomicReference[Option[Boolean]](None)
    val z = AtomicReference[Option[Boolean]](None)
    val _ = sendRecorded(c, 2L, y)                                         // in flight, parked in core.async
    val kz = sendRecorded(c, 3L, z)                                // queued here
    c.cancelSend(kz)
    assertEquals(c.receiveBlocking().map(_.longValue), Some(1L))
    assertEquals(c.receiveBlocking().map(_.longValue), Some(2L))
    c.close()
    assertEquals(c.receiveBlocking(), None)
    assertEquals(z.get, None, "a withdrawn send is not answered either")
  }

  test("close is TWO-PHASE: a send queued before close is still delivered") {
    val c = CoreAsync.channel[java.lang.Long](1)
    assert(c.offer(Long.box(1L)))
    val y = AtomicReference[Option[Boolean]](None)
    val z = AtomicReference[Option[Boolean]](None)
    val _ = sendRecorded(c, 2L, y)                                         // in flight
    val _ = sendRecorded(c, 3L, z)                                         // queued behind it
    c.close()                                                      // refuses NEW sends only
    assertEquals(drain(c).map(_.longValue), List(1L, 2L, 3L))
    assertEquals((y.get, z.get), (Some(true), Some(true)))
    val late = AtomicReference[Option[Boolean]](None)
    val _ = sendRecorded(c, 4L, late)
    assertEquals(late.get, Some(false), "after close a send is refused at once")
  }

  test("finished: false while open or holding elements, true once closed AND empty") {
    val c = CoreAsync.channel[java.lang.Long](4)
    assert(!c.finished)
    assert(c.offer(Long.box(1L)))
    c.close()
    assert(!c.finished, "closed but not empty")
    assertEquals(c.receiveBlocking().map(_.longValue), Some(1L))
    assert(c.finished, "closed and drained")
  }
}
