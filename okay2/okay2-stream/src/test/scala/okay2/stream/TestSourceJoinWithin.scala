package okay2.stream

import java.util.concurrent.atomic.AtomicInteger
import scala.util.Random
import okay2._
import okay2.platform._
import okay2.stream.Source.SourceOps

/**
 * `Source.joinWithin` (specs/stream-join.md, stage 2), the Scala 2 twin
 * of the core's TestSourceJoinWithin: the windowed join on the live
 * carrier — the interval's pairs whatever the merge's interleaving,
 * since the watermark is the smaller side's. No release law here:
 * okay2 has no cancel scope.
 */
class TestSourceJoinWithin extends munit.FunSuite {

  private type Row = (Long, String)

  private def rows[O](s: Source[O]): Vector[O] = s.runCollect.runWith

  /** each side sorted by time — so no row is ever late, and the pair
   * set is exactly the interval's, however the two sides interleave */
  private def side(rnd: Random, n: Int, tag: String): List[(String, Row)] =
    List.fill(n)((rnd.nextInt(5).toString, rnd.between(0L, 200L))).sortBy(_._2).zipWithIndex
      .map { case ((k, t), i) => (k, (t, s"$tag$i")) }

  test("the interval's pairs, at every buffer size, whatever the interleaving") {
    for { capacity <- List(1, 4, 64); seed <- 1 to 6 } {
      val rnd = new Random(seed)
      val l = side(rnd, rnd.nextInt(60), "l"); val r = side(rnd, rnd.nextInt(60), "r")
      val within = 25L
      val expected = (for { (k, a) <- l; (k2, b) <- r if k == k2 && math.abs(a._1 - b._1) <= within } yield (k, a._2, b._2)).sorted
      val out = rows(Source.joinWithin(Source.of(l), Source.of(r), within, 0L, capacity)(_._1, _._1))
      assertEquals(out.map { case (k, (a, b)) => (k, a._2, b._2) }.sorted, expected.toVector, s"capacity $capacity seed $seed")
    }
  }

  test("two endless sides join lazily under an early stop") {
    val ticks = Source.of(LazyList.from(0).map(i => ("k", (i.toLong, s"l$i"))))
    val tocks = Source.of(LazyList.from(0).map(i => ("k", (i.toLong, s"r$i"))))
    val j = Source.joinWithin(ticks, tocks, 0L, 0L, capacity = 4)(_._1, _._1)
    assertEquals(j.runFoldUntil(FoldUntil.take[(String, (Row, Row))](3)).runWith.map { case (k, (a, b)) => (k, a._1, b._1) },
      Vector(("k", 0L, 0L), ("k", 1L, 1L), ("k", 2L, 2L)))
  }

  test("a finite side against an endless one: the join ends once nothing can be produced, and the endless side stops") {
    val produced = new AtomicInteger(0)
    val endless = Source.of(LazyList.from(0).map { i => produced.incrementAndGet(); ("k", (i.toLong, s"r$i")) })
    val j = Source.joinWithin(Source.of(List(("k", (5L, "l5")))), endless, 2L, 0L, capacity = 4)(_._1, _._1)
    val out = rows(j).map { case (k, (a, b)) => (k, a._2, b._2) }
    // the left row at 5 reaches the right at 3..7; once the right has passed 7 the left row is
    // evicted, the left side has ended, nothing can be produced: the stage returns
    assertEquals(out.sorted, Vector.tabulate(5)(i => ("k", "l5", s"r${3 + i}")))
    // no scope to release the feeder, but its buffer fills and it parks: production is BOUNDED — what the join
    // read, plus the buffer, plus the element in hand (15-17 measured, 5 runs). Waited for, not slept on (Settle)
    val (still, last) = Settle.await(produced)
    assert(still, s"the endless side was still producing 10 s after the join ended ($last elements)")
    assert(last <= 32, s"the endless side produced $last elements, past what the join read plus its buffer")
  }
}
