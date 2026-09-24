package okay2.stream

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._
import okay2.async.CanBlock
import okay2.platform._

/**
 * The `Channel` contract as laws every implementation must satisfy —
 * okay-stream's TestChannelLaws (okay2 spec stage 28), law for law, over
 * the same table of implementations and the same claims.
 *
 * TWO TIERS, because the contract has two: the CORE tier is what
 * "channel" means at all (order, no duplication, a closed channel
 * accepts nothing) and every implementation answers for it; the DRAIN
 * tier (acceptance is final, close ends the stream only after the
 * buffer) is `AbruptChannel`'s deliberate refusal, and the suite
 * RECORDS which laws each implementation signed for — an ignored test
 * per unclaimed law, in the gate's own output.
 *
 * The one property of the Scala 3 suite (ScalaCheck's `forAll`) is a
 * seeded loop here: okay2's build has munit and nothing else.
 */
abstract class ChannelLawsSuite(impls: List[(String, Boolean, Int => Channel[Int])]) extends munit.FunSuite {

  private val cb: CanBlock = implicitly[CanBlock]

  private def drains(name: String): Boolean = impls.find(_._1 == name).exists(_._2)

  private def each(name: String)(law: (String, Int => Channel[Int]) => Unit): Unit =
    impls.foreach { case (n, _, mk) => test(s"$name — $n")(law(n, mk)) }

  /** a law that needs more than one consumer: a single-consumer ring does
   * not claim it, by construction */
  private val singleConsumerOnly = Set("SentinelChannel/single-consumer")

  /** the one implementation that gives up EXACT per-producer order: the
   * default, `growing`, across its one swap (the operator's decision of
   * 2026-09-18 in the Scala 3 core) */
  private val swapsItsBuffer = Set("SentinelChannel/growing")

  private def eachOrdered(name: String)(law: (String, Int => Channel[Int]) => Unit): Unit =
    impls.foreach { case (n, _, mk) =>
      val claim = if (swapsItsBuffer(n)) s"$name — EXCEPT once, across its one swap" else name
      test(s"$claim — $n")(law(n, mk))
    }

  private def manyConsumers(name: String)(law: (String, Int => Channel[Int]) => Unit): Unit =
    impls.foreach { case (n, _, mk) =>
      if (singleConsumerOnly(n)) test(s"$name — $n (single consumer: not claimed)".ignore)(())
      else test(s"$name — $n")(law(n, mk))
    }

  private def drainers(name: String)(law: (String, Int => Channel[Int]) => Unit): Unit =
    impls.foreach { case (n, drains, mk) =>
      if (drains) test(s"$name — $n")(law(n, mk))
      else test(s"$name — $n (not claimed)".ignore)(())
    }

  private def drainAll(c: Channel[Int]): List[Int] =
    Iterator.continually(c.receiveBlocking()(cb)).takeWhile(_.isDefined).flatten.toList

  // ── LAW 1: acceptance is final ─────────────────────────────────────
  drainers("law: an accepted element is always delivered, whenever close lands") { (n, mk) =>
    for (round <- 1 to 60) {
      val c = mk(4)
      val accepted = ConcurrentHashMap.newKeySet[Int]()
      val received = ArrayBuffer.empty[Int]
      val p = Thread.ofVirtual().start { () =>
        var i = 0; var on = true
        while (on && i < 3000) {
          if (c.sendBlocking(i)(cb)) { val _ = accepted.add(i) } else on = false
          i += 1
        }
      }
      val q = Thread.ofVirtual().start { () =>
        var go = true
        while (go) c.receiveBlocking()(cb) match {
          case Some(v) => received += v
          case None => go = false
        }
      }
      Thread.sleep(0, (round % 7) * 100000)
      c.close()
      p.join(); q.join()
      assertEquals(received.toSet, accepted.asScala.toSet, s"$n round $round: accepted but not delivered")
    }
  }

  /**
   * LAW 1b — the same law where the window is: close racing a crowd of
   * NON-PARKING offers. Law 1's one parking producer meets close mostly
   * through `sendAsync`'s own open check, so a buffer that decided
   * "open" BEFORE winning its position (Ring.pushDeciding reading the
   * flag ahead of the CAS) survived it — measured, okay2 stage 28: the
   * mutant passed law 1. Four producers offering in a tight loop against
   * a close at a random instant put sends on both sides of the end mark
   * every round, and an element accepted after it is never delivered.
   */
  drainers("law: an accepted element is delivered when close races many offers") { (n, mk) =>
    val rnd = new scala.util.Random(1)
    for (round <- 1 to 300) {
      val c = mk(64)
      val accepted = ConcurrentHashMap.newKeySet[Int]()
      val go = new java.util.concurrent.atomic.AtomicBoolean(true)
      val ps = (0 until 4).map(w => Thread.ofVirtual().start { () =>
        var i = 0
        while (go.get) {
          val v = w * 10000000 + i
          if (c.offer(v)) { val _ = accepted.add(v) }
          i += 1
          if (c.isClosed) go.set(false)
        }
      })
      val received = new ConcurrentLinkedQueue[Int]()
      val q = Thread.ofVirtual().start { () =>
        var more = true
        while (more) c.receiveBlocking()(cb) match {
          case Some(v) => val _ = received.add(v)
          case None => more = false
        }
      }
      Thread.sleep(0, rnd.nextInt(200000))
      c.close()
      ps.foreach(_.join()); q.join()
      val got = received.asScala.toSet
      val lost = accepted.asScala.toSet -- got
      assert(lost.isEmpty, s"$n round $round: ${lost.size} accepted but not delivered, e.g. ${lost.take(3)}")
    }
  }

  // ── LAW 2: the end comes after the buffer, never instead ───────────
  drainers("law: close does not discard what is already buffered (200 seeded lists)") { (n, mk) =>
    val rnd = new scala.util.Random(27)
    for (_ <- 1 to 200) {
      val xs = List.fill(rnd.nextInt(1000))(rnd.nextInt())
      val c = mk(1024)
      xs.foreach(x => assert(c.offer(x), s"$n: offer under capacity refused"))
      c.close()
      assertEquals(drainAll(c), xs, s"$n")
    }
  }

  drainers("law: a receiver sees None only once the buffer is drained") { (n, mk) =>
    val c = mk(8)
    assert(c.offer(1)); assert(c.offer(2))
    c.close()
    assertEquals(c.receiveBlocking()(cb), Some(1))
    assertEquals(c.receiveBlocking()(cb), Some(2))
    assertEquals(c.receiveBlocking()(cb), None)
    assertEquals(c.receiveBlocking()(cb), None, s"$n: the end is stable once reached")
  }

  // ── LAW 3: a closed channel accepts nothing ────────────────────────
  each("law: after close, send answers false and offer refuses") { (n, mk) =>
    val c = mk(64)
    c.close()
    assertEquals(c.sendBlocking(1)(cb), false, s"$n: a closed channel took an element")
    assertEquals(c.offer(2), false, s"$n: a closed channel offered")
    assertEquals(c.receiveBlocking()(cb), None, s"$n")
  }

  // ── LAW 3b: `finished` is the conclusion, not the ingredients ──────
  drainers("law: finished is false while anything remains, true once nothing can arrive") { (n, mk) =>
    val c = mk(8)
    assert(!c.finished, s"$n: an open, empty channel is not finished")
    assert(c.offer(1))
    c.close()
    assert(!c.finished, s"$n: closed but still holding an element")
    assertEquals(c.receiveBlocking()(cb), Some(1))
    assert(c.finished, s"$n: closed and drained")
    assertEquals(c.receiveBlocking()(cb), None)
  }

  drainers("law: an accepted element keeps finished false until it is delivered") { (n, mk) =>
    for (_ <- 1 to 200) {
      val c = mk(4)
      assert(c.offer(7))
      c.close()
      assert(!c.finished, s"$n: finished while an accepted element was outstanding")
      assertEquals(c.receiveBlocking()(cb), Some(7))
      assert(c.finished, s"$n")
    }
  }

  // ── LAW 4: order is preserved per producer ─────────────────────────
  each("law: one producer's elements arrive in the order it sent them") { (n, mk) =>
    val c = mk(16)
    val sent = (1 to 2000).toList
    val p = Thread.ofVirtual().start { () => sent.foreach(i => { val _ = c.sendBlocking(i)(cb) }); c.close() }
    val out = drainAll(c)
    p.join()
    assertEquals(out, sent.take(out.length), s"$n: FIFO per producer")
  }

  eachOrdered("law: TWO producers each arrive in the order they sent") { (n, mk) =>
    // a FRESH consumer thread per round: a partitioned buffer starts each
    // consumer at a part chosen by its thread, and one consumer asks the
    // same rotation every time (the Scala 3 core's measurement)
    val each = (1 to 1000).toList
    var round = 0
    while (round < 5) {
      val c = mk(16)
      val live = new AtomicInteger(2)
      val ps = (0 to 1).map(p => Thread.ofVirtual().start { () =>
        each.foreach(i => { val _ = c.sendBlocking(2 * i + p)(cb) })
        if (live.decrementAndGet() == 0) c.close()
      })
      var out = List.empty[Int]
      val consumer = Thread.ofVirtual().start { () => out = drainAll(c) }
      consumer.join()
      ps.foreach(_.join())
      (0 to 1).foreach { p =>
        val own = out.filter(_ % 2 == p)
        val sent = each.map(2 * _ + p)
        if (!swapsItsBuffer(n))
          assertEquals(own, sent.take(own.length), s"$n: round $round, producer $p out of its own order")
        else {
          // the WEAKENED law: one displacement, once — not a licence to reorder
          val inversions = own.lazyZip(own.drop(1)).count { case (a, b) => a > b }
          assert(inversions <= 1, s"$n: round $round, producer $p had $inversions inversions: ${own.take(40)}")
          assertEquals(own.sorted, own.sorted.distinct, s"$n: round $round, producer $p duplicated")
          assert(own.forall(sent.contains), s"$n: round $round, producer $p invented an element")
        }
      }
      round += 1
    }
  }

  // ── a failure records, it does not close ───────────────────────────
  each("law: fail records without closing — a healthy producer still sends") { (n, mk) =>
    val c = mk(16)
    assert(c.offer(1), s"$n: offer before the failure")
    c.fail(new RuntimeException("boom"))
    assert(!c.isClosed, s"$n: fail closed the channel")
    assert(c.offer(2), s"$n: a healthy producer was silenced by another's failure")
    assertEquals(c.failed.map(_.getMessage), Some("boom"), s"$n: the failure was not recorded")
  }

  drainers("law: a failure is the END — everything accepted arrives before it") { (n, mk) =>
    val c = mk(16)
    assert(c.offer(1))
    c.fail(new RuntimeException("boom"))
    assert(c.offer(2))
    c.close()
    assertEquals(c.receiveBlocking()(cb), Some(1), s"$n: buffered before the failure")
    assertEquals(c.receiveBlocking()(cb), Some(2), s"$n: sent after the failure")
    val thrown = intercept[RuntimeException](c.receiveBlocking()(cb))
    assertEquals(thrown.getMessage, "boom", s"$n: the failure is the end")
  }

  // ── the bulk send is the elementwise one, batched ──────────────────
  each("law: sendManyNow takes a prefix of what it was offered, losing and duplicating nothing") { (n, mk) =>
    for (_ <- 1 to 20) {
      val c = mk(64)
      val per = 1000
      val ps = (0 until 2).map(w => Thread.ofVirtual().start { () =>
        var i = 0
        while (i < per) {
          val room = math.min(16, per - i)
          val base = w * per + i
          val took = c.sendManyNow(room)(j => base + j)
          // PARK on a refusal, never retry in a loop (a loaded box starved the consumer)
          if (took == 0) { val _ = c.sendBlocking(base)(cb); i += 1 } else i += took
        }
      })
      val seen = ArrayBuffer.empty[Int]
      val q = Thread.ofVirtual().start { () =>
        while (seen.length < 2 * per) c.receiveBlocking()(cb) match {
          case Some(v) => seen += v
          case None => seen += -1
        }
      }
      ps.foreach(_.join()); q.join(); c.close()
      assertEquals(seen.length, 2 * per, s"$n: bulk send count")
      assertEquals(seen.toSet, (0 until 2 * per).toSet, s"$n: bulk send lost or duplicated")
    }
  }

  // ── the bulk receive is the elementwise one, batched ───────────────
  manyConsumers("law: receiveMany takes each element exactly once, under contending consumers") { (n, mk) =>
    for (_ <- 1 to 20) {
      val c = mk(64)
      val total = 3000
      val p = Thread.ofVirtual().start { () =>
        var i = 0
        while (i < total) { val _ = c.sendBlocking(i)(cb); i += 1 }
      }
      val seen = new ConcurrentLinkedQueue[Int]()
      val counted = new AtomicInteger(0)
      val qs = (0 until 2).map(_ => Thread.ofVirtual().start { () =>
        while (counted.get < total) {
          val chunk = cb.block[Either[Throwable, Chunk[Int]]] { k => c.receiveManyAsync(64)(k); () => () }.fold(throw _, identity)
          var i = 0
          while (i < chunk.length) { val _ = seen.add(chunk(i)); i += 1 }
          val _ = counted.addAndGet(chunk.length)
        }
      })
      p.join()
      while (counted.get < total) Thread.`yield`()
      c.close()
      qs.foreach(_.join())
      val got = seen.asScala.toList
      assertEquals(got.length, got.toSet.size, s"$n: bulk receive duplicated")
      assertEquals(got.toSet, (0 until total).toSet, s"$n: bulk receive lost")
    }
  }

  // ── LAW 5: nothing is duplicated, whatever the interleaving ────────
  each("law: many producers, one consumer — no loss, no duplication") { (n, mk) =>
    val c = mk(8)
    val per = 500
    val ps = (0 until 4).map(k => Thread.ofVirtual().start { () =>
      (0 until per).foreach(i => { val _ = c.sendBlocking(k * per + i)(cb) })
    })
    val got = ArrayBuffer.empty[Int]
    val q = Thread.ofVirtual().start { () =>
      var go = true
      while (go) c.receiveBlocking()(cb) match {
        case Some(v) => got += v
        case None => go = false
      }
    }
    ps.foreach(_.join()); c.close(); q.join()
    assertEquals(got.length, got.toSet.size, s"$n: duplicated")
    assert(got.forall(v => v >= 0 && v < 4 * per), s"$n: invented an element")
    if (drains(n)) assertEquals(got.toSet, (0 until 4 * per).toSet, s"$n: contents")
  }
}

class TestChannelLaws extends ChannelLawsSuite(TestChannelLaws.impls)

object TestChannelLaws {
  /** the implementation, and whether it promises drain-on-close */
  val impls: List[(String, Boolean, Int => Channel[Int])] = List(
    ("StmChannel", true, cap => new StmChannel[Int](cap)),
    ("SentinelChannel", true, cap => new SentinelChannel[Int](cap)),
    ("SentinelChannel/unbounded", true, _ => new SentinelChannel[Int](new Segments[Any]())),
    ("SentinelChannel/adaptive", true, cap => Queues.strong[Int].adaptive.parts(4).each(math.max(8, cap)).build),
    ("SentinelChannel/relaxed", true, cap => Queues.strong[Int].relaxed.parts(4).each(math.max(2, cap)).build),
    // THE DEFAULT answers here too — the Scala 3 core's lesson: the one
    // mechanism every caller gets was once the one these laws never ran
    ("SentinelChannel/growing", true, cap => Queues.strong[Int].growing(math.max(2, cap), parts = 8).build),
    ("SentinelChannel/single-consumer", true, cap => Queues.strong[Int].bounded(cap, singleConsumer = true).build),
    ("AbruptChannel", false, cap => new AbruptChannel[Int](cap)),
  )
}
