package okay

/**
 * feed-offer-first: the feed offers in a loop and parks only on the
 * element the ring refused. The laws are about the refusal — the one
 * moment an element is in the producer's hand and not yet in the ring.
 */
class TestFeedOfferFirst extends munit.FunSuite:

  private def collect(src: Source[Int]): Vector[Int] =
    val out = Vector.newBuilder[Int]
    src.runForeach(x => okay.effect[Async, Unit](Async.Run(() => out += x))).runWith
    out.result()

  test("law: an element the ring refused is not lost -- every element arrives, in order, through a ring of 2") {
    // capacity 2 against 5000 elements: the ring refuses thousands of
    // times, and each refusal is exactly the element held in hand
    val n = 5000
    assertEquals(collect(Channel.buffer(2)((0 until n).toList).drained), (0 until n).toVector)
  }

  test("law: the same through the unbounded and the STM-backed channels") {
    val n = 3000
    assertEquals(collect(Channel.buffer(Int.MaxValue)((0 until n).toList).drained), (0 until n).toVector)
    assertEquals(collect(Channel.buffer(1)((0 until n).toList).drained), (0 until n).toVector)
  }

  test("law: a consumer that stops early leaves the producer parked on a refusal, not spinning -- and close releases it") {
    val c = Channel[Int](2)
    val producer = Async.spawn(okay.pure(()).flatMap(_ =>
      // the same loop feed runs, spelled out: offer until refused, then one send
      def go(i: Int): Unit ! Async =
        if i >= 100 then okay.pure(())
        else if c.offer(i) then go(i + 1)
        else c.send(i).flatMap(ok => if ok then go(i + 1) else okay.pure(()))
      go(0)))
    assertEquals(c.receiveBlocking(), Some(0))       // take one; the producer is now parked on a refusal
    Thread.sleep(20)
    c.close()                                        // close under the producer
    producer.join()                                  // the parked send answers false and the loop ends
    assert(c.isClosed)
  }
