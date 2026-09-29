package okay2.stream

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import okay2._
import okay2.async._
import okay2.platform._
import okay2.stream.Source.SourceOps

/**
 * `Source.zip` (specs/source-zip.md), the Scala 2 twin of the core's
 * TestSourceZip: two live sources paired in lockstep, each side on a
 * fiber of its own, the pairing on the consumer's thread.
 */
class TestSourceZip extends munit.FunSuite {

  private type R = Writer[Int] + Async

  private def pairs[A, B](s: Source[(A, B)]): Vector[(A, B)] = s.runCollect.runWith

  test("lockstep pairs, ending at the shorter side — whichever side that is, at every buffer size") {
    for (capacity <- List(1, 4, 64)) {
      assertEquals(
        pairs(Source.of(List(1, 2, 3)).zip(Source.of(List("a", "b")), capacity)),
        Vector((1, "a"), (2, "b")), s"capacity $capacity: right shorter")
      assertEquals(
        pairs(Source.of(List("a", "b")).zip(Source.of(List(1, 2, 3)), capacity)),
        Vector(("a", 1), ("b", 2)), s"capacity $capacity: left shorter")
      assertEquals(pairs(Source.of(List.empty[Int]).zip(Source.of(List(1)), capacity)), Vector.empty[(Int, Int)])
      assertEquals(pairs(Source.of(List(1)).zip(Source.of(List.empty[Int]), capacity)), Vector.empty[(Int, Int)])
    }
  }

  test("each side keeps its own order, however the two sides' buffers align") {
    val n = 2000L
    val out = pairs(Source.range(0, n).zip(Source.of(LazyList.range(0L, n)), capacity = 7))
    assertEquals(out.size, n.toInt)
    assert(out.forall { case (a, b) => a == b }, "a pair out of step")
    assertEquals(out.map(_._1), Vector.range(0L, n))
  }

  test("zipWith folds the pair as it is told") {
    val s = Source.of(List(1, 2, 3)).zipWith(Source.of(List(10, 20, 30)))(_ + _)
    assertEquals(s.runCollect.runWith, Vector(11, 22, 33))
  }

  test("two infinite sources zip lazily under an early stop") {
    val z = Source.of(LazyList.from(0)).zip(Source.of(LazyList.from(100)), capacity = 4)
    assertEquals(z.runFoldUntil(FoldUntil.take[(Int, Int)](5)).runWith, Vector.tabulate(5)(i => (i, 100 + i)))
  }

  test("the side that outlives the other is closed at the end: its feeder, parked on the full buffer, ends") {
    // the right side is endless and its feeder parks on a buffer of 4;
    // the left side ends after one element. The feeder's thread is
    // remembered by an Async step at the source's front, which runs on
    // the feeder's own fiber; the proof it ended is that it is gone
    val produced = new AtomicInteger(0)
    @volatile var feeder: Thread = null
    val endless: Source[Int] =
      Async { feeder = Thread.currentThread() }.at[R]
        .flatMap(_ => Source.of(LazyList.from(0).map { i => produced.incrementAndGet(); i }))
    assertEquals(pairs(Source.of(List(7)).zip(endless, capacity = 4)), Vector((7, 0)))
    val t = feeder
    assert(t != null, "the feeder never ran")
    t.join(TimeUnit.SECONDS.toMillis(10))
    assert(!t.isAlive, s"the survivor's feeder is still parked after the zip ended (produced ${produced.get})")
    // the one element paired, the buffer, the slot the pairing freed
    // and refilled, the element the full buffer then refused
    assert(produced.get <= 1 + 4 + 1 + 1, s"the endless side ran on after the zip ended: ${produced.get}")
  }

  test("a side that fails fails the zip, after every pair told before the failure") {
    object Boom extends RuntimeException("boom")
    val failing: Source[Int] =
      Source.of(List(1, 2)).flatMap(_ => Async[Unit](throw Boom).at[R])
    val seen = Vector.newBuilder[(Int, Int)]
    val z = Source.of(LazyList.from(10)).zip(failing, capacity = 64)
    val thrown = intercept[RuntimeException](z.runForeach(p => Async { seen += p; () }).runWith)
    assert(thrown eq Boom, s"wrong failure: $thrown")
    assertEquals(seen.result(), Vector((10, 1), (11, 2)))
  }
}
