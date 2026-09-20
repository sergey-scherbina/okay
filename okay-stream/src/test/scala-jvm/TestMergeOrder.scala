package okay

/**
 * THE LAW A MERGE CAN STATE EXACTLY (channel-known-producers): each
 * side's elements come out in the order that side told them — not
 * "except once, across the swap", which is the default channel's
 * weakened claim (TestChannelLaws), because a merge knows it has two
 * producers and builds two parts from the start, so nothing is ever
 * adopted and no swap exists.
 *
 * Rounds with a FRESH CONSUMER THREAD each, for the reason the
 * channel laws give: a partitioned buffer starts a consumer's scan
 * at a part chosen from its thread, so one consumer asks the same
 * rotation every time and can miss a reorder that another rotation
 * shows. `OKAY_MERGE_ROUNDS` raises the count for a loaded-box run
 * (the growing arm broke only under load, thousands of rounds apart);
 * the default keeps the gate short.
 */
class TestMergeOrder extends munit.FunSuite {

  private val rounds: Int =
    Option(System.getenv("OKAY_MERGE_ROUNDS")).flatMap(_.toIntOption).getOrElse(20)

  private def sides(out: List[Int]): (List[Int], List[Int]) =
    (out.filter(_ % 2 == 0), out.filter(_ % 2 == 1))

  test("Channel.merge: each side arrives in exactly the order it sent") {
    val n = 1000
    val evens = LazyList.range(0, n).map(_ * 2)
    val odds = LazyList.range(0, n).map(_ * 2 + 1)
    var round = 0
    while round < rounds do
      val c = Channel.merge(evens, odds, capacity = 16)
      var out = List.empty[Int]
      val consumer = Thread.ofVirtual().start { () =>
        out = Iterator.continually(c.receiveBlocking()).takeWhile(_.isDefined).flatten.toList
      }
      consumer.join()
      val (e, o) = sides(out)
      assertEquals(e, evens.toList, s"round $round: the even side came back out of order")
      assertEquals(o, odds.toList, s"round $round: the odd side came back out of order")
      round += 1
  }

  test("Source.merge: each side keeps its own order, elementwise and chunked") {
    val n = 1000
    val evens = Source.of(LazyList.range(0, n).map(_ * 2))
    val odds = Source.of(LazyList.range(0, n).map(_ * 2 + 1))
    var round = 0
    while round < rounds do
      val chunked = round % 2 == 1
      var out = List.empty[Int]
      val consumer = Thread.ofVirtual().start { () =>
        out = evens.merge(odds, capacity = 16, chunked = chunked).runCollect.runWith.toList
      }
      consumer.join()
      val (e, o) = sides(out)
      assertEquals(e, (0 until n).map(_ * 2).toList, s"round $round (chunked=$chunked): even side")
      assertEquals(o, (0 until n).map(_ * 2 + 1).toList, s"round $round (chunked=$chunked): odd side")
      round += 1
  }

  test("Channel.buffer: one producer, the order it sent, nothing lost") {
    val n = 5000
    val out = Channel.buffer(8)(LazyList.range(0, n)).drained.runCollect.runWith
    assertEquals(out.toList, (0 until n).toList)
  }
}
