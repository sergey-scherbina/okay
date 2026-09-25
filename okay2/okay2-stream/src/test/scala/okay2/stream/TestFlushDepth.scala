package okay2.stream

import okay2._
import okay2.async._
import okay2.platform._
import okay2.stream.Source.SourceOps

/** stack-safety-stream-stm: 200 000 empty flushes in a row must not be a
 * stack overflow (okay-stream's TestFlushDepth) */
class TestFlushDepth extends munit.FunSuite {
  type R = Flush + (Writer[Int] + Async)
  def tell(i: Int): Free[R, Unit] = Writer.tell(i)
  def empty: Flushing[Int] = pure[R, Unit](())
  def flush: Free[R, Unit] = Flush.now

  test("200 000 empty flushes in a row, then one element") {
    val n = 200000
    val flushes = (1 to n).foldLeft(pure[R, Unit](()))((m, _) => m.flatMap(_ => flush))
    val p: Flushing[Int] = flushes.flatMap(_ => tell(1))
    assertEquals(p.mergeFlushing(empty).toLazyList.toList, List(1))
  }

  test("a flush between every one of 100 000 elements keeps them all, in order") {
    val n = 100000
    val p: Flushing[Int] = (1 to n).foldLeft(pure[R, Unit](()))((m, i) =>
      m.flatMap(_ => tell(i)).flatMap(_ => flush))
    val out = p.mergeFlushing(empty).toLazyList.toList
    assertEquals(out.length, n)
    assertEquals(out.head, 1)
    assertEquals(out.last, n)
  }
}
