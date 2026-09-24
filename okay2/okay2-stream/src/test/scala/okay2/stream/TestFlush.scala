package okay2.stream

import java.util.concurrent.{CompletableFuture, TimeUnit}
import okay2._
import okay2.async._
import okay2.platform._
import okay2.stream.Source.SourceOps

/** A producer's own chunk boundary — the Scala 3 core's `Flush` tests
 * (TestStreamSource, TestChunkEdges): the boundary lands where the
 * producer put it, full chunks still form around it, and the edges
 * (nothing buffered, twice in a row, just before the end) say nothing */
class TestFlush extends munit.FunSuite {

  type R = Flush + (Writer[Int] + Async)
  def tell(i: Int): Free[R, Unit] = Writer.tell(i)
  val empty: Flushing[Int] = pure[R, Unit](())
  def flush: Free[R, Unit] = Flush.now

  /** three elements (fewer than a chunk), a boundary, then a source
   * that never ends: only the Flush can get them out */
  def marked(base: Int): Flushing[Int] =
    tell(base + 1).flatMap(_ => tell(base + 2)).flatMap(_ => tell(base + 3)).flatMap(_ => flush)
      .flatMap(_ => (Async(Thread.sleep(60000)): Free[R, Unit]))

  def within[X](what: => X): X = CompletableFuture.supplyAsync(() => what).get(10, TimeUnit.SECONDS)

  test("Flush.now emits a partial chunk at the producer's own boundary, with no timer at all") {
    val got = within(marked(0).mergeFlushing(marked(0)).toLazyList.take(6).toList)
    assertEquals(got.sorted, List(1, 1, 2, 2, 3, 3))
  }

  test("Flush.now is a boundary, not a fence: full chunks still form around it") {
    val many: Flushing[Int] = (1 to 20).foldLeft(pure[R, Unit](())) { (m, i) =>
      m.flatMap(_ => tell(i)).flatMap(_ => if (i == 18) flush else pure[R, Unit](()))
    }
    assertEquals(many.mergeFlushing(empty).toLazyList.toList, (1 to 20).toList)
  }

  test("eitherFlushing: mergeFlushing's boundaries, elements tagged by side") {
    val got = within(marked(0).eitherFlushing(marked(100)).toLazyList.take(6).toList)
    assertEquals(got.collect { case Left(i) => i }.sorted, List(1, 2, 3))
    assertEquals(got.collect { case Right(i) => i }.sorted, List(101, 102, 103))
  }

  test("the edges: nothing buffered, twice in a row, just before the end, an empty source") {
    val first: Flushing[Int] = flush.flatMap(_ => tell(1)).flatMap(_ => tell(2))
    assertEquals(first.mergeFlushing(empty).toLazyList.toList, List(1, 2))
    val last: Flushing[Int] = tell(1).flatMap(_ => tell(2)).flatMap(_ => flush)
    assertEquals(last.mergeFlushing(empty).toLazyList.toList, List(1, 2))
    val twice: Flushing[Int] = tell(1).flatMap(_ => flush).flatMap(_ => flush).flatMap(_ => tell(2))
    assertEquals(twice.mergeFlushing(empty).toLazyList.toList, List(1, 2))
    assertEquals(empty.mergeFlushing(empty).toLazyList.toList, Nil)
  }

  test("an ordinary Source IS a Flushing one: it merges with a flushing source unchanged") {
    val plain: Source[Int] = Source.of((1 to 5).toList)
    // widened by ASCRIPTION, nothing rebuilt: the row is contravariant.
    // Ascribed because a plain Source's type does not mention Flush, so
    // Flush's companion (and its operations) is not in its implicit scope
    assertEquals((plain: Flushing[Int]).mergeFlushing(empty).toLazyList.toList, (1 to 5).toList)
  }

  test("Flush.map leaves the marks where they were") {
    val got = within(Flush.map(marked(0))(_ * 10).mergeFlushing(empty).toLazyList.take(3).toList)
    assertEquals(got, List(10, 20, 30))
  }
}
