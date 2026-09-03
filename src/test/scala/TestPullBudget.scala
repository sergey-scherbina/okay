package okay

import scala.annotation.nowarn

/**
 * chunk-stack-safety: a stage that accumulates without emitting takes
 * `through`'s producer/stage handshake once per element, and only an
 * emission unwinds the stack. Before the budget these overflowed —
 * 4096 over 4000 elements, and any chunk a short stream cannot fill.
 */
class TestPullBudget extends munit.FunSuite {

  test("a chunk larger than the whole stream: everything held, flushed at the end") {
    for (n, k) <- List((4000, 4096), (3000, 4096), (4000, 8192), (5000, 4096),
                       (20000, 65536)) do
      val out = Source.of(LazyList.range(0L, n.toLong)).chunked(k).toLazyList.toList
      assertEquals(out.map(_.length).sum, n, s"n=$n k=$k")
      assertEquals(out.length, math.ceil(n.toDouble / k).toInt, s"chunk count at n=$n k=$k")
      assertEquals(out.init.forall(_.length == k), true, s"only the last is short, n=$n k=$k")
  }

  test("large chunks that DO fill, repeatedly") {
    val out = Source.of(LazyList.range(0L, 50000L)).chunked(8192).toLazyList.toList
    assertEquals(out.map(_.length).sum, 50000)
    assertEquals(out.init.forall(_.length == 8192), true)
  }

  test("the same depth through a merge, where the stage sits behind a channel") {
    val a = Source.of(LazyList.range(0L, 5000L))
    val b = Source.of(LazyList.range(5000L, 10000L))
    assertEquals(a.chunked(4096).merge(b.chunked(4096)).unchunked.toLazyList.length, 10000)
  }

  // Writer.run's inline body checks the answer at the chunk type,
  // abstract here — the trusted kernel's warning (Effects.scala), not
  // a cast this file adds
  @nowarn("msg=cannot be checked at runtime")
  def runChunks(told: Int ! Writer % Int, k: Int): Seq[Chunk[Int]] =
    !.run(Writer.run(through(told)(Stage.chunked[Int](k))))._1

  test("a plain producer through an accumulating stage overflows no more") {
    // the Writer-only overload, which has no channel and no Async
    val told: Int ! Writer % Int =
      (1 to 5000).foldLeft(pure[Writer % Int, Int](0))((m, i) =>
        m.flatMap(_ => Writer.tell(i).map(_ => i)))
    assertEquals(runChunks(told, 8192).map(_.length).sum, 5000)
  }
}
