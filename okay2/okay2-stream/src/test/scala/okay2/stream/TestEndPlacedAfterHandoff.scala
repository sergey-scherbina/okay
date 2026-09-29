package okay2.stream

import scala.collection.mutable.ArrayBuffer

/**
 * An end mark placed AFTER a handoff wakes the receiver that registered in
 * between — the Scala 3 core's TestEndPlacedAfterHandoff (okay-stream),
 * ported with the fix (sentinel-single-consumer-lost-end, 2026-09-29).
 *
 * One thread, nested callbacks: the ring is full when close lands, so the
 * end is left pending; each answer's callback is the consumer's next
 * receive, and the third registers on an empty ring before the second
 * one's placement puts the mark in. Before the fix it was never answered.
 */
class TestEndPlacedAfterHandoff extends munit.FunSuite {

  private val channels: List[(String, () => SentinelChannel[Int])] = List(
    "SentinelChannel" -> (() => new SentinelChannel[Int](2)),
    "SentinelChannel/single-consumer" -> (() => new SentinelChannel[Int](new Ring[Any](2, singleConsumer = true))))

  channels.foreach { case (name, mk) =>
    test(s"$name: a receive registered before a late end placement is woken by it") {
      val c = mk()
      assert(c.offer(1)); assert(c.offer(2))
      c.close()
      val got = ArrayBuffer.empty[Either[Throwable, Option[Int]]]
      c.receiveAsync { r1 =>
        got += r1
        c.receiveAsync { r2 =>
          got += r2
          c.receiveAsync { r3 => got += r3; () }
        }
      }
      assertEquals(got.toList, List(Right(Some(1)), Right(Some(2)), Right(None)),
        s"$name: the third receive was never answered")
    }
  }
}
