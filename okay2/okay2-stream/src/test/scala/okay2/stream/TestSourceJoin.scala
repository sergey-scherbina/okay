package okay2.stream

import java.util.concurrent.atomic.AtomicInteger
import scala.util.Random
import okay2._
import okay2.async._
import okay2.platform._
import okay2.stream.Chunks.ChunksOps
import okay2.stream.Source.SourceOps

/**
 * `Source.joinSorted` (specs/stream-join.md), the Scala 2 twin of the
 * core's TestSourceJoin: the sort-merge join on the live carrier, each
 * side on a fiber of its own; no early-stop release law here, as okay2
 * has no cancel scope.
 */
class TestSourceJoin extends munit.FunSuite {

  private type R = Writer[(Int, String)] + Async

  private def rows[O](s: Source[O]): Vector[O] = s.runCollect.runWith

  private def sortedRows(rnd: Random, n: Int, keys: Int, tag: String): List[(Int, String)] =
    List.fill(n)(rnd.nextInt(keys)).sorted.zipWithIndex.map { case (k, i) => (k, s"$tag$i") }

  private val l3 = List((1, "a"), (2, "b"), (4, "d"))
  private val r4 = List((2, "x"), (3, "y"), (3, "z"), (4, "w"))

  test("the same pairs as Chunks.joinSorted, at every buffer size; left and full") {
    for { capacity <- List(1, 4, 64); seed <- 1 to 6 } {
      val rnd = new Random(seed)
      val l = sortedRows(rnd, rnd.nextInt(80), 1 + rnd.nextInt(10), "l")
      val r = sortedRows(rnd, rnd.nextInt(80), 1 + rnd.nextInt(10), "r")
      val expected = Chunks.joinSorted(Chunks.fromIterator(l.iterator), Chunks.fromIterator(r.iterator)).elements.toVector
      assertEquals(rows(Source.joinSorted(Source.of(l), Source.of(r), capacity)), expected, s"capacity $capacity seed $seed")
    }
    assertEquals(rows(Source.leftJoinSorted(Source.of(l3), Source.of(r4))),
      Vector((1, ("a", None)), (2, ("b", Some("x"))), (4, ("d", Some("w")))))
    assertEquals(rows(Source.fullJoinSorted(Source.of(l3), Source.of(r4))),
      Vector((1, (Some("a"), None)), (2, (Some("b"), Some("x"))),
             (3, (None, Some("y"))), (3, (None, Some("z"))), (4, (Some("d"), Some("w")))))
  }

  test("an inner join ends at the left's end and closes the endless right, whose feeder stops producing") {
    val produced = new AtomicInteger(0)
    val endless: Source[(Int, Int)] = Source.of(LazyList.from(0).map { i => produced.incrementAndGet(); (i, i) })
    assertEquals(rows(Source.joinSorted(Source.of(List((1, "a"))), endless, capacity = 4)), Vector((1, ("a", 1))))
    assert(produced.get <= 3 + 4 + 1 + 1, s"the endless side ran on after the join ended: ${produced.get}")
    val settled = produced.get
    Thread.sleep(50)
    assertEquals(produced.get, settled, "the endless side is still producing after the join ended")
    val endlessL: Source[(Int, String)] = Source.of(LazyList.from(0).map(i => (i, s"l$i")))
    assertEquals(rows(Source.joinSorted(endlessL, Source.of(List((0, "x"), (2, "y"))), capacity = 4)),
      Vector((0, ("l0", "x")), (2, ("l2", "y"))))
  }

  test("a side that fails fails the join, after every pair told before the failure; so does a key out of order") {
    object Boom extends RuntimeException("boom")
    val failing: Source[(Int, String)] =
      Source.of(List((0, "x"), (1, "y"))).flatMap(_ => Async[Unit](throw Boom).at[R])
    val seen = Vector.newBuilder[(Int, (Int, String))]
    val j = Source.joinSorted(Source.of(LazyList.from(0).map(i => (i, i))), failing, capacity = 64)
    val thrown = intercept[RuntimeException](j.runForeach(p => Async { seen += p; () }).runWith)
    assert(thrown eq Boom, s"wrong failure: $thrown")
    assertEquals(seen.result(), Vector((0, (0, "x"))))
    val seen2 = Vector.newBuilder[(Int, (String, String))]
    val j2 = Source.joinSorted(Source.of(List((0, "a"), (2, "b"))), Source.of(List((0, "x"), (2, "y"), (1, "z"))))
    val e = intercept[IllegalArgumentException](j2.runForeach(p => Async { seen2 += p; () }).runWith)
    assert(e.getMessage.contains("right side") && e.getMessage.contains("key 1 after 2"), e.getMessage)
    assertEquals(seen2.result(), Vector((0, ("a", "x"))))
  }
}
