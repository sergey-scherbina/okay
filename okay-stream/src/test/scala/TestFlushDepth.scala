package okay

/**
 * stack-safety-stream-stm: the flushing feed walks the producer's program
 * with one native frame per step that sends nothing — and a `Flush.now`
 * with nothing buffered sends nothing. A poller that flushes after every
 * empty poll is exactly that, so 200 000 empty flushes in a row must not
 * be a stack overflow. (A `tell` descends at most `Source.ChunkSize` deep
 * before a send breaks the descent; a flush had no such bound.)
 */
class TestFlushDepth extends munit.FunSuite:
  private type R = Flush + (Writer % Int + Async)
  private def tell(i: Int): Unit ! R = okay.effect[R, Unit](Writer(i))
  private def empty: Flushing[Int] = okay.pure[R, Unit](())

  test("200 000 empty flushes in a row, then one element") {
    val n = 200000
    val flushes = (1 to n).foldLeft(okay.pure[R, Unit](()))((m, _) => m.flatMap(_ => Flush.now[Writer % Int + Async]))
    val p: Flushing[Int] = flushes.flatMap(_ => tell(1))
    assertEquals(p.mergeFlushing(empty).toLazyList.toList, List(1))
  }

  test("a flush between every one of 100 000 elements keeps them all, in order") {
    val n = 100000
    val p: Flushing[Int] = (1 to n).foldLeft(okay.pure[R, Unit](()))((m, i) =>
      m.flatMap(_ => tell(i)).flatMap(_ => Flush.now[Writer % Int + Async]))
    val out = p.mergeFlushing(empty).toLazyList.toList
    assertEquals(out.length, n)
    assertEquals(out.head, 1)
    assertEquals(out.last, n)
  }
