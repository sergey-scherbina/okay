package okay

/**
 * An end mark placed AFTER a handoff wakes the receiver that registered
 * in between (sentinel-single-consumer-lost-end, 2026-09-29).
 *
 * The whole-build sighting: a consumer PARKED on a closed channel whose
 * ring held the end mark (`hasReady=true`, `receivers=1`, `metEnds=0`).
 * The mark was placed by `placeEnd`, which publishes into the ring and
 * wakes nobody. That is safe while `placeEnd` runs on the consumer's own
 * path, before the consumer looks again. It is not safe after a HANDOFF:
 * a resumed receive runs on the waker's thread, answers `k` first and
 * places the end second, and in that gap the consumer is free to take
 * its answer, look again, find the ring empty and park. Nobody is left
 * to wake it: close's own wake has already run, and the placement never
 * wakes.
 *
 * Here the gap is made deterministic with nested callbacks, one thread:
 * the ring is full when close lands (so close cannot seal it), and each
 * answer's callback is the consumer's next receive. The third receive
 * registers on an empty ring before the second one's `placeEnd` puts the
 * mark in. Before the fix its callback never ran.
 */
class TestEndPlacedAfterHandoff extends munit.FunSuite with okay.testkit.Munit.Diagnosed {

  private val channels: List[(String, () => SentinelChannel[Int])] = List(
    "SentinelChannel" -> (() => SentinelChannel[Int](2)),
    "SentinelChannel/single-consumer" ->
      (() => SentinelChannel[Int](Ring[Int | Mark](2, singleConsumer = true))))

  for (name, mk) <- channels do
    test(s"$name: a receive registered before a late end placement is woken by it") {
      val c = mk()
      assert(c.offer(1)); assert(c.offer(2))
      c.close()   // full: close cannot seal, the end is left pending
      note(s"after close: ${c.debugState}")
      val got = scala.collection.mutable.ArrayBuffer.empty[Either[Throwable, Option[Int]]]
      c.receiveAsync { r1 =>
        got += r1
        c.receiveAsync { r2 =>
          got += r2
          // the ring is empty and the end not yet placed: this receive
          // registers, and the placement that follows r2 must wake it
          c.receiveAsync { r3 => got += r3 }
          note(s"third receive registered: ${c.debugState}")
        }
      }
      note(s"after the handoffs: ${c.debugState}")
      assertEquals(got.toList, List(Right(Some(1)), Right(Some(2)), Right(None)),
        s"$name: the third receive was never answered, ${c.debugState}")
    }
}
