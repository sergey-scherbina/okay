package okay

import org.scalacheck.Prop.forAll
import scala.jdk.CollectionConverters.*

/**
 * The `Channel` contract, as laws every implementation must satisfy —
 * written because the interface named its operations and said nothing
 * about what must be true of them, so two implementations
 * (ring-channel) each rediscovered the same invariants by failing a
 * full gate one run in three.
 *
 * Parameterised over the implementation on purpose: a new mechanism
 * earns its place by passing these, not by surviving a gate a few
 * times. `StmChannel` is the reference, and it is the reference
 * because its single atomic transition gives the invariants for free.
 */
class TestChannelLaws extends munit.ScalaCheckSuite {

  /** every implementation under test, by name */
  private val impls: List[(String, Int => Channel[Int])] = List(
    "StmChannel" -> (cap => StmChannel[Int](cap)),
    // add a mechanism here and it must answer for the whole contract.
    // These were checked against the withdrawn CasChannel with its
    // in-flight fix reverted, and law 1 failed in 0.05s naming itself
    // -- the same defect the FULL GATE caught roughly one run in
    // three. That is the point of writing them down.
  )

  private def each(name: String)(law: (String, Int => Channel[Int]) => Unit): Unit =
    impls.foreach((n, mk) => test(s"$name — $n")(law(n, mk)))

  // ── LAW 1: acceptance is final ──────────────────────────────────
  // If send answered true, that element WILL be delivered. This is
  // the law all four ring-channel bugs broke, and the reason the
  // producer/consumer/close shape is a law rather than one test.

  each("law: an accepted element is always delivered, whenever close lands") { (n, mk) =>
    for round <- 1 to 60 do
      val c = mk(4)
      val accepted = java.util.concurrent.ConcurrentHashMap.newKeySet[Int]()
      val received = scala.collection.mutable.ArrayBuffer.empty[Int]
      val p = Thread.ofVirtual().start { () =>
        var i = 0; var on = true
        while on && i < 3000 do
          if c.sendBlocking(i) then accepted.add(i): Unit else on = false
          i += 1
      }
      val q = Thread.ofVirtual().start { () =>
        var go = true
        while go do c.receiveBlocking() match
          case Some(v) => received += v
          case None => go = false
      }
      Thread.sleep(0, (round % 7) * 100000)
      c.close()
      p.join(); q.join()
      assertEquals(received.toSet, accepted.asScala.toSet, s"$n round $round: accepted but not delivered")
  }

  // ── LAW 2: the end comes after the buffer, never instead ─────────

  // a PROPERTY, not a test: `each` wraps `test`, which discards the
  // `Prop` that `forAll` answers with -- so writing this as `each`
  // silently checked nothing. The compiler said so (discarded
  // non-Unit value of type Prop) and it was right
  impls.foreach: (n, mk) =>
    property(s"law: close does not discard what is already buffered — $n") {
      forAll { (xs: List[Int]) =>
        val c = mk(1024)
        val fit = xs.take(1000)
        fit.foreach(x => c.offer(x): Unit)
        c.close()
        val out = Iterator.continually(c.receiveBlocking()).takeWhile(_.isDefined).flatten.toList
        out == fit
      }
    }

  each("law: a receiver sees None only once the buffer is drained") { (n, mk) =>
    val c = mk(8)
    assert(c.offer(1)); assert(c.offer(2))
    c.close()
    assertEquals(c.receiveBlocking(), Some(1))
    assertEquals(c.receiveBlocking(), Some(2))
    assertEquals(c.receiveBlocking(), None)
    assertEquals(c.receiveBlocking(), None, s"$n: the end is stable once reached")
  }

  // ── LAW 3: a closed channel accepts nothing ──────────────────────

  each("law: after close, send answers false and offer refuses") { (n, mk) =>
    val c = mk(64)
    c.close()
    assertEquals(c.sendBlocking(1), false, s"$n: a closed channel took an element")
    assertEquals(c.offer(2), false, s"$n: a closed channel offered")
    assertEquals(c.receiveBlocking(), None, s"$n")
  }

  // ── LAW 3b: `finished` is the conclusion, not the ingredients ────

  each("law: finished is false while anything remains, true once nothing can arrive") { (n, mk) =>
    val c = mk(8)
    assert(!c.finished, s"$n: an open, empty channel is not finished")
    assert(c.offer(1))
    c.close()
    assert(!c.finished, s"$n: closed but still holding an element")
    assertEquals(c.receiveBlocking(), Some(1))
    assert(c.finished, s"$n: closed and drained")
    assertEquals(c.receiveBlocking(), None)
  }

  each("law: an accepted element keeps finished false until it is delivered") { (n, mk) =>
    // the shape the ring-channel defects lived in: an element already
    // accepted must keep the channel unfinished, so no consumer can
    // conclude the stream ended while it is still owed
    for _ <- 1 to 200 do
      val c = mk(4)
      assert(c.offer(7))
      c.close()
      assert(!c.finished, s"$n: finished while an accepted element was outstanding")
      assertEquals(c.receiveBlocking(), Some(7))
      assert(c.finished, s"$n")
  }

  // ── LAW 4: order is preserved per producer ───────────────────────

  each("law: one producer's elements arrive in the order it sent them") { (n, mk) =>
    val c = mk(16)
    val sent = (1 to 2000).toList
    val p = Thread.ofVirtual().start { () =>
      sent.foreach(i => { val _ = c.sendBlocking(i) }); c.close()
    }
    val out = Iterator.continually(c.receiveBlocking()).takeWhile(_.isDefined).flatten.toList
    p.join()
    assertEquals(out, sent, s"$n: FIFO per producer")
  }

  // ── LAW 5: nothing is duplicated, whatever the interleaving ──────

  each("law: many producers, one consumer — no loss, no duplication") { (n, mk) =>
    val c = mk(8)
    val per = 500
    val ps = (0 until 4).map(k => Thread.ofVirtual().start { () =>
      (0 until per).foreach(i => { val _ = c.sendBlocking(k * per + i) })
    })
    val got = scala.collection.mutable.ArrayBuffer.empty[Int]
    val q = Thread.ofVirtual().start { () =>
      var go = true
      while go do c.receiveBlocking() match
        case Some(v) => got += v
        case None => go = false
    }
    ps.foreach(_.join()); c.close(); q.join()
    assertEquals(got.length, 4 * per, s"$n: count")
    assertEquals(got.toSet, (0 until 4 * per).toSet, s"$n: contents")
  }
}
