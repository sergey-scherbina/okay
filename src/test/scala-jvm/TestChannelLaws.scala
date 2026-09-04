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
 *
 * The laws come in TWO TIERS, because the contract itself does. The
 * core tier is what "channel" means at all — order, no duplication,
 * a closed channel accepts nothing more — and every implementation
 * answers for it. The DRAIN tier is the stronger promise `StmChannel`
 * makes and `AbruptChannel` deliberately refuses: acceptance is
 * final, and close ends the channel only after the buffer is spent.
 *
 * Splitting them is what makes the trade a fact rather than a
 * comment. `AbruptChannel` does not "fail" five laws; it does not
 * claim them, and the suite records which laws each implementation
 * signed up for.
 */
class TestChannelLaws extends munit.ScalaCheckSuite {

  /** does this implementation promise that a closed channel still
   * yields what it already accepted? */
  private val impls: List[(String, Boolean, Int => Channel[Int])] = List(
    ("StmChannel", true, cap => StmChannel[Int](cap)),
    ("AbruptChannel", false, cap => AbruptChannel[Int](cap)),
    // add a mechanism here and it must answer for the whole contract.
    // These were checked against the withdrawn CasChannel with its
    // in-flight fix reverted, and law 1 failed in 0.05s naming itself
    // -- the same defect the FULL GATE caught roughly one run in
    // three. That is the point of writing them down.
  )

  private def drains(name: String): Boolean = impls.find(_._1 == name).exists(_._2)

  /** a law of the core tier: every implementation answers for it */
  private def each(name: String)(law: (String, Int => Channel[Int]) => Unit): Unit =
    impls.foreach((n, _, mk) => test(s"$name — $n")(law(n, mk)))

  /** a law of the drain tier: only implementations that promise it.
   * The others are RECORDED as ignored rather than silently skipped,
   * so the gate's own output says which guarantees each mechanism
   * signed for -- an implementation that stops claiming one has to
   * edit the table above, where it is visible. */
  private def drainers(name: String)(law: (String, Int => Channel[Int]) => Unit): Unit =
    impls.foreach: (n, drains, mk) =>
      if drains then test(s"$name — $n")(law(n, mk))
      else test(s"$name — $n (not claimed)".ignore)(())

  // ── LAW 1: acceptance is final ──────────────────────────────────
  // If send answered true, that element WILL be delivered. This is
  // the law all four ring-channel bugs broke, and the reason the
  // producer/consumer/close shape is a law rather than one test.

  drainers("law: an accepted element is always delivered, whenever close lands") { (n, mk) =>
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
  impls.filter(_._2).foreach: (n, _, mk) =>
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

  drainers("law: a receiver sees None only once the buffer is drained") { (n, mk) =>
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

  drainers("law: finished is false while anything remains, true once nothing can arrive") { (n, mk) =>
    val c = mk(8)
    assert(!c.finished, s"$n: an open, empty channel is not finished")
    assert(c.offer(1))
    c.close()
    assert(!c.finished, s"$n: closed but still holding an element")
    assertEquals(c.receiveBlocking(), Some(1))
    assert(c.finished, s"$n: closed and drained")
    assertEquals(c.receiveBlocking(), None)
  }

  drainers("law: an accepted element keeps finished false until it is delivered") { (n, mk) =>
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
    // ORDER, not completeness: a channel that discards on close still
    // owes that what it DID deliver came in the order it was sent, so
    // the law reads as a prefix and the drain tier owns the tail
    assertEquals(out, sent.take(out.length), s"$n: FIFO per producer")
  }

  // ── LAW: the bulk receive is the elementwise one, batched ────────
  // A batched primitive that loses or reorders is worse than none:
  // it fails only under load, which is where it is used. Two
  // consumers make the bulk claim contend with itself, which is the
  // case the single-CAS-per-batch scan exists for.

  each("law: receiveMany takes each element exactly once, under contending consumers") { (n, mk) =>
    for _ <- 1 to 20 do
      val c = mk(64)
      val total = 3000
      val p = Thread.ofVirtual().start { () =>
        var i = 0
        while i < total do { val _ = c.sendBlocking(i); i += 1 }
      }
      val seen = java.util.concurrent.ConcurrentLinkedQueue[Int]()
      val counted = java.util.concurrent.atomic.AtomicInteger(0)
      val cb = summon[CanBlock]
      val qs = (0 until 2).map(_ => Thread.ofVirtual().start { () =>
        while counted.get < total do
          val chunk = cb.block[Either[Throwable, Chunk[Int]]] { k =>
            c.receiveManyAsync(64)(k); () => ()
          }.fold(throw _, identity)
          var i = 0
          while i < chunk.length do { seen.add(chunk(i)): Unit; i += 1 }
          counted.addAndGet(chunk.length): Unit
      })
      p.join()
      // the extra consumer is parked on a channel that will never
      // fill again -- only close can release it, and it must not
      // happen before the count is in, or the drain-tier channel
      // would be asked to deliver what it already delivered
      while counted.get < total do Thread.onSpinWait()
      c.close()
      qs.foreach(_.join())
      val got = seen.asScala.toList
      assertEquals(got.length, got.toSet.size, s"$n: bulk receive duplicated")
      assertEquals(got.toSet, (0 until total).toSet, s"$n: bulk receive lost")
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
    // no duplication and nothing invented: true of ANY channel. The
    // count is the drain tier's claim -- and it must be asserted from
    // the table, not from whether a race happened to leave the buffer
    // empty at close, which is how this law passed by luck once
    assertEquals(got.length, got.toSet.size, s"$n: duplicated")
    assert(got.forall(v => v >= 0 && v < 4 * per), s"$n: invented an element")
    if drains(n) then
      assertEquals(got.toSet, (0 until 4 * per).toSet, s"$n: contents")
  }
}
