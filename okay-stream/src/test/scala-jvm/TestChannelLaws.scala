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
/**
 * The laws, over ANY list of implementations — `TestChannelLaws` runs them
 * over okay-stream's own, and a module that implements `Channel` elsewhere
 * (okay-clojure's view of a core.async channel) runs the SAME battery by
 * extending this with its own list (clojure-core-async, 2026-09-23). The
 * Boolean is the DRAIN tier: whether the implementation signs up for
 * drain-on-close.
 */
abstract class ChannelLawsSuite(impls: List[(String, Boolean, Int => Channel[Int])]) extends munit.ScalaCheckSuite {


  private def drains(name: String): Boolean = impls.find(_._1 == name).exists(_._2)

  /** a law of the core tier: every implementation answers for it */
  private def each(name: String)(law: (String, Int => Channel[Int]) => Unit): Unit =
    impls.foreach((n, _, mk) => test(s"$name — $n")(law(n, mk)))

  /** a law of the drain tier: only implementations that promise it.
   * The others are RECORDED as ignored rather than silently skipped,
   * so the gate's own output says which guarantees each mechanism
   * signed for -- an implementation that stops claiming one has to
   * edit the table above, where it is visible. */
  /** a law that needs MORE THAN ONE consumer: an implementation built
   * on the promise of exactly one does not claim it, and says so in
   * the gate's output the way an un-drained one does */
  private val singleConsumerOnly = Set("SentinelChannel/single-consumer")

  /**
   * THE IMPLEMENTATIONS THAT GIVE UP EXACT PER-PRODUCER ORDER, and
   * there is exactly one: the default (operator's decision,
   * 2026-09-18). `growing` ADOPTS the ring producers were already
   * pushing into, so a producer's elements can straddle the one-shot
   * swap — and `AdaptiveFifo.popManyAdoptedFirst` orders the read to
   * put them back, per CALL, with a window between the two reads
   * inside it (`adopted-window`). What it costs is ONE displacement
   * per producer per swap, and that is now what this states rather
   * than a promise the buffer does not keep.
   *
   * EVERY OTHER MECHANISM HERE STILL CLAIMS THE EXACT LAW, which is
   * the point of naming this set instead of weakening the law for
   * everyone: `adaptive` never adopts (`first` is null, so `adopted`
   * is false) and every producer's elements live in one part, and the
   * plain ring orders every push on one tail. A caller who needs the
   * exact order asks for one of those BY NAME, and docs/queues.md
   * says how.
   */
  private val swapsItsBuffer = Set("SentinelChannel/growing")

  /** an ordering law whose NAME says which claim was checked, because
   * one implementation signs for a weaker one and a gate line reading
   * "each arrive in the order they sent" against a buffer that does
   * not promise that is the kind of half-truth this file exists to
   * prevent */
  private def eachOrdered(name: String)(law: (String, Int => Channel[Int]) => Unit): Unit =
    impls.foreach: (n, _, mk) =>
      val claim = if swapsItsBuffer(n) then s"$name — EXCEPT once, across its one swap" else name
      test(s"$claim — $n")(law(n, mk))
  private def manyConsumers(name: String)(law: (String, Int => Channel[Int]) => Unit): Unit =
    impls.foreach: (n, _, mk) =>
      if singleConsumerOnly(n) then test(s"$name — $n (single consumer: not claimed)".ignore)(())
      else test(s"$name — $n")(law(n, mk))

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

  /**
   * LAW 1b — the same law where its window is: close racing a crowd of
   * NON-PARKING offers (channel-law-racing-offers, 2026-09-24).
   *
   * Law 1's one parking producer meets close mostly through the channel's
   * own open check before the claim, so a buffer that decided "open"
   * BEFORE winning its position — `Ring.pushDeciding` reading the flag
   * ahead of the tail CAS, the exact window claim-then-decide exists to
   * close — passed it. Found in okay2 (spec stage 29), where the port of
   * this suite let that mutant through; measured again HERE on the same
   * mutant before this law was added. Four producers offering in a tight
   * loop against a close at a random instant put sends on both sides of
   * the end mark every round, and an element accepted after it is never
   * delivered.
   */
  drainers("law: an accepted element is delivered when close races many offers") { (n, mk) =>
    val rnd = scala.util.Random(1)
    for round <- 1 to 300 do
      val c = mk(64)
      val accepted = java.util.concurrent.ConcurrentHashMap.newKeySet[Int]()
      val go = java.util.concurrent.atomic.AtomicBoolean(true)
      val ps = (0 until 4).map(w => Thread.ofVirtual().start { () =>
        var i = 0
        while go.get do
          val v = w * 10000000 + i
          if c.offer(v) then accepted.add(v): Unit
          i += 1
          if c.isClosed then go.set(false)
      })
      val received = java.util.concurrent.ConcurrentLinkedQueue[Int]()
      val q = Thread.ofVirtual().start { () =>
        var more = true
        while more do c.receiveBlocking() match
          case Some(v) => received.add(v): Unit
          case None => more = false
      }
      Thread.sleep(0, rnd.nextInt(200000))
      c.close()
      ps.foreach(_.join()); q.join()
      val lost = accepted.asScala.toSet -- received.asScala.toSet
      assert(lost.isEmpty, s"$n round $round: ${lost.size} accepted but not delivered, e.g. ${lost.take(3)}")
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

  eachOrdered("law: TWO producers each arrive in the order they sent") { (n, mk) =>
    // one producer is not enough to state this law, and that gap is
    // how a reordering default shipped: a buffer that partitions by
    // producer has nothing to partition until there are two, so with
    // one it is only ever the plain ring underneath
    // (merge-chunked-order, 2026-09-09).
    // A FRESH CONSUMER THREAD PER ROUND, and that is not decoration.
    // A partitioned buffer gives each consumer a starting part from
    // its thread's identity hash, so ONE consumer asks the same
    // rotation every time -- and a consumer that happens to start at
    // the part holding the older elements reads them first and sees
    // nothing wrong. Measured on the defect this law was written for:
    // one drain on the test's own thread caught it in one run out of
    // three; five rounds on fresh threads caught it in every run.
    val each = (1 to 1000).toList
    var round = 0
    while round < 5 do
      val c = mk(16)
      val live = java.util.concurrent.atomic.AtomicInteger(2)
      val ps = (0 to 1).map(p => Thread.ofVirtual().start { () =>
        each.foreach(i => { val _ = c.sendBlocking(2 * i + p) })
        if live.decrementAndGet() == 0 then c.close()
      })
      var out = List.empty[Int]
      val consumer = Thread.ofVirtual().start { () =>
        out = Iterator.continually(c.receiveBlocking()).takeWhile(_.isDefined).flatten.toList
      }
      consumer.join()
      ps.foreach(_.join())
      // a prefix per producer, for the same reason law 4 reads as one
      (0 to 1).foreach { p =>
        val own = out.filter(_ % 2 == p)
        val sent = each.map(2 * _ + p)
        if !swapsItsBuffer(n) then
          assertEquals(own, sent.take(own.length),
            s"$n: round $round, producer $p out of its own order")
        else
          // THE WEAKENED LAW, and it is not a licence to reorder.
          // A swap is ONE-SHOT and displaces a producer's stragglers
          // as a block, so its own sequence can fall out of order in
          // at most ONE place. The mass reordering this buffer had
          // before `popManyAdoptedFirst` (73 rounds in 300, and a
          // source coming back `1..16, 49, 50, 17..48`) shows up as
          // MANY inversions and still fails here.
          val inversions = own.lazyZip(own.drop(1)).count((a, b) => a > b)
          assert(inversions <= 1,
            s"$n: round $round, producer $p had $inversions inversions; a one-shot " +
              s"swap can displace its stragglers once, not repeatedly: ${own.take(40)}")
          // and nothing may be invented or lost, whatever the order
          assertEquals(own.sorted, own.sorted.distinct, s"$n: round $round, producer $p duplicated")
          assert(own.forall(sent.contains), s"$n: round $round, producer $p invented an element")
      }
      round += 1
  }

  // ── LAW: a failure records, it does not close ────────────────────
  // Caught by the full gate and not by these laws, which is why it is
  // one now. `Channel.merge` feeds one channel from two sources: if
  // one fails and that silences the other, the healthy source's
  // elements are lost. The failure belongs at the END of the stream,
  // after everything already accepted has been handed over.

  each("law: fail records without closing — a healthy producer still sends") { (n, mk) =>
    val c = mk(16)
    assert(c.offer(1), s"$n: offer before the failure")
    c.fail(RuntimeException("boom"))
    assert(!c.isClosed, s"$n: fail closed the channel")
    assert(c.offer(2), s"$n: a healthy producer was silenced by another's failure")
    assertEquals(c.failed.map(_.getMessage), Some("boom"), s"$n: the failure was not recorded")
  }

  // the rest of it is the drain tier's: a channel that discards on
  // close cannot promise that what was accepted arrives BEFORE the
  // failure does, because it promises nothing arrives after close
  drainers("law: a failure is the END — everything accepted arrives before it") { (n, mk) =>
    val c = mk(16)
    assert(c.offer(1), s"$n: offer before the failure")
    c.fail(RuntimeException("boom"))
    assert(c.offer(2), s"$n: offer after the failure")
    c.close()
    assertEquals(c.receiveBlocking(), Some(1), s"$n: buffered before the failure")
    assertEquals(c.receiveBlocking(), Some(2), s"$n: sent after the failure")
    val thrown = intercept[RuntimeException](c.receiveBlocking())
    assertEquals(thrown.getMessage, "boom", s"$n: the failure is the end")
  }

  // ── LAW: the bulk send is the elementwise one, batched ───────────
  // Two producers offering runs into the same ring make the bulk
  // claim contend with itself, which is the case the single-CAS
  // scan exists for. Partial acceptance is the contract, so a
  // producer retries what did not fit.

  each("law: sendManyNow takes a prefix of what it was offered, losing and duplicating nothing") { (n, mk) =>
    for _ <- 1 to 20 do
      val c = mk(64)
      val per = 1000
      val ps = (0 until 2).map(w => Thread.ofVirtual().start { () =>
        var i = 0
        while i < per do
          val room = math.min(16, per - i)
          val base = w * per + i
          val took = c.sendManyNow(room)(j => base + j)
          // PARK, do not retry in a loop. Two producers retrying
          // against a 64-slot buffer burn every carrier they are
          // given, and the consumer they are waiting for gets none:
          // on a quiet box this passed, on a loaded one it took the
          // matrix past thirteen minutes in this one law. Yielding
          // instead of spinning was not enough, because the retry
          // loop itself is the cost. A blocking send parks, which is
          // also how a caller would really write this.
          if took == 0 then { val _ = c.sendBlocking(base); i += 1 }
          else i += took
      })
      val seen = scala.collection.mutable.ArrayBuffer.empty[Int]
      val q = Thread.ofVirtual().start { () =>
        while seen.length < 2 * per do
          c.receiveBlocking() match
            case Some(v) => seen += v
            case None => seen += -1
      }
      ps.foreach(_.join()); q.join(); c.close()
      assertEquals(seen.length, 2 * per, s"$n: bulk send count")
      assertEquals(seen.toSet, (0 until 2 * per).toSet, s"$n: bulk send lost or duplicated")
  }

  // ── LAW: the bulk receive is the elementwise one, batched ────────
  // A batched primitive that loses or reorders is worse than none:
  // it fails only under load, which is where it is used. Two
  // consumers make the bulk claim contend with itself, which is the
  // case the single-CAS-per-batch scan exists for.

  manyConsumers("law: receiveMany takes each element exactly once, under contending consumers") { (n, mk) =>
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
      while counted.get < total do Thread.`yield`()
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

class TestChannelLaws extends ChannelLawsSuite(TestChannelLaws.impls)

object TestChannelLaws {

  /** does this implementation promise that a closed channel still
   * yields what it already accepted? */
  val impls: List[(String, Boolean, Int => Channel[Int])] = List(
    ("StmChannel", true, cap => StmChannel[Int](cap)),
    ("SentinelChannel", true, cap => SentinelChannel[Int](cap)),
    // the unbounded one answers for the SAME laws: it ignores the
    // capacity because it has none, and a law that leans on a full
    // buffer must still hold when the buffer never fills
    ("SentinelChannel/unbounded", true, _ => SentinelChannel[Int](Segments[Int | Mark]())),
    // the RELAXED one answers for every law here, which is the point
    // of the tier split: what it gives up is the order BETWEEN
    // producers, and no law states that. Drain-on-close it still
    // owes, and keeps only because close seals every part
    // the adaptive one answers for every law too: it may choose its
    // part count freely, and no law it must keep depends on that
    ("SentinelChannel/adaptive", true,
      cap => Queues.strong[Int].adaptive.parts(4).each(math.max(8, cap)).build),
    ("SentinelChannel/relaxed", true,
      cap => Queues.strong[Int].relaxed.parts(4).each(math.max(2, cap)).build),
    // THE DEFAULT ANSWERS HERE TOO, and did not until 2026-09-09.
    // `growing` became `Channel.apply`'s buffer on 2026-09-08 and was
    // never added to this list, so the one mechanism every caller
    // gets by default was the one mechanism these laws never ran. It
    // was breaking law 4 the whole time — for two producers, which is
    // the only width at which it partitions at all
    // (merge-chunked-order).
    ("SentinelChannel/growing", true,
      cap => Queues.strong[Int].growing(math.max(2, cap), parts = 8).build),
    // the single-consumer ring answers for every law but the one with
    // contending consumers, which it declines by construction (see
    // `oneConsumer` below): its head moves by a store, not a CAS
    ("SentinelChannel/single-consumer", true,
      cap => Queues.strong[Int].bounded(cap, singleConsumer = true).build),
    ("AbruptChannel", false, cap => AbruptChannel[Int](cap)),
    // add a mechanism here and it must answer for the whole contract.
    // These were checked against the withdrawn CasChannel with its
    // in-flight fix reverted, and law 1 failed in 0.05s naming itself
    // -- the same defect the FULL GATE caught roughly one run in
    // three. That is the point of writing them down.
  )
}
