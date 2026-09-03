package okay

/**
 * The edges of chunked merging and of `Flush` — the sizes and shapes
 * where an off-by-one in the buffer, a missed final flush or a lost
 * boundary would hide. `Source.ChunkSize` is 16, so the interesting
 * counts are around it and around zero.
 */
class TestChunkEdges extends munit.FunSuite {

  private def ints(r: Range): Source[Int] = Source.of(r.toList)
  private def drain(s: Source[Int]): List[Int] = s.toLazyList.toList

  // ── sizes around the chunk boundary ──────────────────────────────

  for n <- List(0, 1, 15, 16, 17, 31, 32, 33) do
    test(s"chunked merge of $n elements a side: nothing lost, nothing duplicated") {
      val out = drain(ints(1 to n).merge(ints(n + 1 to 2 * n), chunked = true))
      assertEquals(out.length, 2 * n, s"count at n=$n")
      assertEquals(out.toSet, (1 to 2 * n).toSet, s"contents at n=$n")
    }

  test("each source's own order survives chunking, whatever the interleaving") {
    val out = drain(ints(1 to 50).merge(ints(101 to 150), chunked = true))
    assertEquals(out.filter(_ <= 50), (1 to 50).toList)
    assertEquals(out.filter(_ > 100), (101 to 150).toList)
  }

  // ── asymmetry: one side empty, one side short ───────────────────

  test("one empty side contributes nothing and does not stall the other") {
    assertEquals(drain(ints(1 to 20).merge(ints(1 to 0), chunked = true)), (1 to 20).toList)
    assertEquals(drain(ints(1 to 0).merge(ints(1 to 20), chunked = true)), (1 to 20).toList)
  }

  test("both sides empty: an empty stream, not a hang") {
    assertEquals(drain(ints(1 to 0).merge(ints(1 to 0), chunked = true)), Nil)
  }

  test("a side shorter than one chunk still flushes at its end") {
    val out = drain(ints(1 to 3).merge(ints(100 to 140), chunked = true))
    assertEquals(out.toSet, ((1 to 3) ++ (100 to 140)).toSet)
  }

  // ── the consumer that stops early ───────────────────────────────

  test("an early-stopping consumer gets its elements and the merge does not wedge") {
    val got = ints(1 to 200).merge(ints(1001 to 1200), chunked = true).toLazyList.take(5).toList
    assertEquals(got.length, 5)
  }

  // ── failure ─────────────────────────────────────────────────────

  test("a failing source does not swallow what the other one produced") {
    val boom = new RuntimeException("boom")
    val bad: Source[Int] =
      Source.of(List(1, 2)).flatMap(_ =>
        !.widen[Unit, Async, Writer % Int](
          okay.effect[Async, Unit](Async.Run(() => throw boom))))
    val out = scala.util.Try(drain(bad.merge(ints(100 to 130), chunked = true)))
    // either it surfaces the failure or it delivers the good side —
    // what it must NOT do is hang or lose the good side silently
    out match
      case scala.util.Success(xs) => assert(xs.exists(_ >= 100), s"good side lost: $xs")
      case scala.util.Failure(e) => var r: Throwable = e
        while r.getCause != null && r.getMessage != "boom" do r = r.getCause
        assertEquals(r.getMessage, "boom")
  }

  // ── Flush at the edges ──────────────────────────────────────────

  private type R = Flush + (Writer % Int + Async)
  private def tell(i: Int): Unit ! R = okay.effect[R, Unit](Writer(i))
  private def empty: Flushing[Int] = okay.pure[R, Unit](())

  test("Flush.now with nothing buffered emits nothing and is not an error") {
    val p: Flushing[Int] = Flush.now[Writer % Int + Async]
      .flatMap(_ => tell(1)).flatMap(_ => tell(2))
    assertEquals(p.mergeFlushing(empty).toLazyList.toList, List(1, 2))
  }

  test("Flush.now immediately before the end does not double the last chunk") {
    val p: Flushing[Int] = tell(1).flatMap(_ => tell(2))
      .flatMap(_ => Flush.now[Writer % Int + Async])
    assertEquals(p.mergeFlushing(empty).toLazyList.toList, List(1, 2))
  }

  test("two Flush.now in a row: the second has nothing to say") {
    val p: Flushing[Int] = tell(1)
      .flatMap(_ => Flush.now[Writer % Int + Async])
      .flatMap(_ => Flush.now[Writer % Int + Async])
      .flatMap(_ => tell(2))
    assertEquals(p.mergeFlushing(empty).toLazyList.toList, List(1, 2))
  }

  test("Flush.now inside a full chunk splits it exactly there") {
    // 20 elements with a boundary after the 3rd: the boundary chunk is
    // short, the rest still chunks normally, order is preserved
    val p: Flushing[Int] = (1 to 20).foldLeft(okay.pure[R, Unit](())): (m, i) =>
      m.flatMap(_ => tell(i))
        .flatMap(_ => if i == 3 then Flush.now[Writer % Int + Async] else okay.pure(()))
    assertEquals(p.mergeFlushing(empty).toLazyList.toList, (1 to 20).toList)
  }

  test("an empty flushing source on both sides is an empty stream") {
    assertEquals(empty.mergeFlushing(empty).toLazyList.toList, Nil)
  }

  test("Source.range tells exactly the half-open range, and is lazy") {
    assertEquals(Source.range(0, 5).toLazyList.toList, List(0L, 1L, 2L, 3L, 4L))
    assertEquals(Source.range(3, 3).toLazyList.toList, Nil)
    assertEquals(Source.range(5, 3).toLazyList.toList, Nil)
    // lazy: an endless range is fine as long as the consumer stops
    assertEquals(Source.range(0, Long.MaxValue).toLazyList.take(4).toList,
      List(0L, 1L, 2L, 3L))
    // and it is the same stream as the collection form
    assertEquals(Source.range(0, 40).toLazyList.toList,
      Source.of(LazyList.range(0L, 40L)).toLazyList.toList)
  }

  // ── the orthogonal combinators ──────────────────────────────────

  test("chunked/unchunked compose to the identity, at every edge size") {
    for n <- List(0, 1, 15, 16, 17, 33) do
      assertEquals(drain(ints(1 to n).chunked().unchunked), (1 to n).toList, s"n=$n")
  }

  test("chunked emits full chunks plus a short final one") {
    val sizes = ints(1 to 35).chunked(16).toLazyList.toList.map(_.length)
    assertEquals(sizes, List(16, 16, 3))
    assertEquals(ints(1 to 32).chunked(16).toLazyList.toList.map(_.length), List(16, 16))
    assertEquals(ints(1 to 0).chunked(16).toLazyList.toList, Nil)
  }

  test("composing chunked with merge is the same stream as the fused flag") {
    val composed = ints(1 to 40).chunked().merge(ints(41 to 80).chunked())
      .unchunked.toLazyList.toList
    val fused = drain(ints(1 to 40).merge(ints(41 to 80), chunked = true))
    assertEquals(composed.toSet, fused.toSet)
    assertEquals(composed.length, fused.length)
  }

  test("chunked composes with buffer, which needs no flag of its own") {
    val buffered = Channel.buffer(4)(ints(1 to 50).chunked(8))
    assertEquals(buffered.toLazyList.toList.flatten, (1 to 50).toList)
  }

  test("the documented chunk-size limit: modest sizes are safe at any length") {
    // the guard on the KNOWN LIMIT in `chunked`'s doc — a stage that
    // accumulates without emitting recurses per element in `through`,
    // so this pins the range the combinator is documented to serve
    for k <- List(16, 64, 256, 1024) do
      val n = 5000
      val out = Source.of(LazyList.range(0L, n.toLong)).chunked(k).toLazyList.toList
      assertEquals(out.map(_.length).sum, n, s"k=$k")
      assertEquals(out.init.forall(_.length == k), true, s"full chunks at k=$k")
  }

  test("the same chunking stage VALUE driven twice does not share its buffer") {
    // a Stage is a VALUE, and `mergeFlushing` drives one `chunker`
    // for both sides — so a chunking stage must not carry state
    // between runs. Free today (the accumulator is immutable), and
    // the guard on any future attempt to make it a mutable buffer,
    // which chunk-size-representation tried and declined
    val st = Stage.chunked[Int](8)
    val a = through(ints(1 to 20))(
      !.widen[Unit, Take % Int + Writer % Chunk[Int], Async](st)).toLazyList.toList
    val b = through(ints(101 to 120))(
      !.widen[Unit, Take % Int + Writer % Chunk[Int], Async](st)).toLazyList.toList
    assertEquals(a.flatten, (1 to 20).toList)
    assertEquals(b.flatten, (101 to 120).toList)
    assertEquals(a.map(_.length), List(8, 8, 4))
  }

  test("chunks are independent objects: an earlier one is not overwritten by a later") {
    val all = ints(1 to 64).chunked(8).toLazyList.toList
    assertEquals(all.length, 8)
    // read them AFTER the whole stream is built: a chunk must own its
    // storage, so a shared or reused buffer would show the last
    // chunk's contents in every slot
    assertEquals(all.map(_.head), List(1, 9, 17, 25, 33, 41, 49, 57))
    assertEquals(all.flatten, (1 to 64).toList)
  }

  // ── flushAfter at the edges ─────────────────────────────────────

  test("flushAfter on a source that ends first: the end wins, no extra chunk") {
    val out = drain(ints(1 to 5).merge(ints(6 to 10), chunked = true, flushAfter = Some(10)))
    assertEquals(out.toSet, (1 to 10).toSet)
    assertEquals(out.length, 10)
  }

  test("flushAfter far longer than the run changes nothing") {
    val out = drain(ints(1 to 100).merge(ints(101 to 200), chunked = true,
      flushAfter = Some(30000)))
    assertEquals(out.length, 200)
    assertEquals(out.toSet, (1 to 200).toSet)
  }
}
