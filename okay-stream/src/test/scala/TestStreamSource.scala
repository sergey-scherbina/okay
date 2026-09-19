package okay

/** The stream combinators that are about a SOURCE — merge, chunked
 * merge and its flush — which left `TestStream` in the core with the
 * `Source` machinery (core-modules stage 1). What stayed there is the
 * `Stream` typeclass itself and its carriers. */
class TestStreamSource extends munit.FunSuite {


  test("unchunked is expand: chunk boundaries leave no trace in the elements") {
    // the property the merge lane depends on — the same elements in
    // the same order whatever the chunk size (merge-chunk-size-curve-
    // inverted rewired this onto `Writer.expand`)
    val src: Source[Int] = Source.of(LazyList.range(0, 50))
    for size <- List(1, 3, 16, 64) do
      assertEquals(src.chunked(size).unchunked.toLazyList.toList, (0 until 50).toList,
        s"chunk size $size changed the elements")
  }

  test("a Source is a stream in Async — the instance merge asks for") {
    val src: Source[Int] = Source(1, 2, 3)
    assertEquals(src.toLazyList.toList, List(1, 2, 3))
    // and the instance is summonable at the type lambda, which is
    // what a combinator taking `Stream[S, F]` needs
    val St = summon[Stream[[W] =>> Unit ! Writer % W + Async, Async]]
    assertEquals(St.uncons(src).runWith.map(_._1), Some(1))
  }

  test("merge(chunked): the same elements as merge, both sources drained, union typed") {
    val a: Source[Int] = Source.of((1 to 50).toList)
    val b: Source[String] = Source.of((51 to 100).map(_.toString).toList)

    // the elements are exactly merge's, whatever the interleaving
    val chunked = a.merge(b, chunked = true).toLazyList.toList
    val plain = Source.of((1 to 50).toList).merge(Source.of((51 to 100).map(_.toString).toList))
      .toLazyList.toList
    assertEquals(chunked.length, 100)
    assertEquals(chunked.toSet, plain.toSet)
    // the union survives: each element is one side or the other
    assertEquals(chunked.collect { case i: Int => i }.sorted, (1 to 50).toList)
    assertEquals(chunked.collect { case s: String => s.toInt }.sorted, (51 to 100).toList)
    // and EACH source's own order is preserved within the merge --
    // both sides, and the plain merge too. Only the first of these
    // four was asserted, and only the first caught the day the
    // channel's buffer stopped keeping a producer's order
    // (merge-chunked-order, 2026-09-09); the plain merge was breaking
    // 21 times in 500 at its own default capacity with nothing to
    // say so.
    assertEquals(chunked.collect { case i: Int => i }, (1 to 50).toList)
    assertEquals(chunked.collect { case s: String => s.toInt }, (51 to 100).toList)
    assertEquals(plain.collect { case i: Int => i }, (1 to 50).toList)
    assertEquals(plain.collect { case s: String => s.toInt }, (51 to 100).toList)
  }

  test("merge(chunked): a partial final chunk is flushed, not dropped") {
    // fewer elements than one chunk on each side: nothing is emitted
    // until the source ENDS, and the flush must not drop them
    val out = Source.of((1 to 7).toList).merge(Source.of((8 to 12).toList), chunked = true)
      .toLazyList.toList
    assertEquals(out.length, 12)
    assertEquals(out.toSet, (1 to 12).toSet)
    // and a size that straddles the chunk boundary either way
    val straddle = Source.of((1 to 40).toList).merge(Source.of((41 to 45).toList), chunked = true)
      .toLazyList.toList
    assertEquals(straddle.toSet, (1 to 45).toSet)
  }

  test("merge(chunked): an empty source contributes nothing and does not hang") {
    val out = Source.of(List.empty[Int]).merge(Source.of(List(1, 2, 3)), chunked = true)
      .toLazyList.toList
    assertEquals(out, List(1, 2, 3))
    assertEquals(Source.of(List.empty[Int]).merge(Source.of(List.empty[Int]), chunked = true)
      .toLazyList.toList, List.empty[Int])
  }

  test("merge(chunked) without flushAfter STALLS a partial chunk — the hazard, shown") {
    // three elements is fewer than one chunk, and the source never
    // ends, so nothing can trigger an emission
    def trickle: Source[Int] =
      Source.of(List(1, 2, 3)).flatMap(_ =>
        !.widen[Unit, Async, Writer % Int](Async.sleep(60000)))
    val merged = trickle.merge(trickle, chunked = true)
    val f = java.util.concurrent.CompletableFuture.supplyAsync(() =>
      merged.toLazyList.take(3).toList)
    val stalled = intercept[java.util.concurrent.TimeoutException](
      f.get(500, java.util.concurrent.TimeUnit.MILLISECONDS))
    assert(stalled != null)   // the wait expired: nothing was emitted
    val _ = f.cancel(true)
  }

  test("merge(chunked, flushAfter) delivers a partial chunk from a source that never ends") {
    def trickle: Source[Int] =
      Source.of(List(1, 2, 3)).flatMap(_ =>
        !.widen[Unit, Async, Writer % Int](Async.sleep(60000)))
    val merged = trickle.merge(trickle, chunked = true, flushAfter = Some(50))
    val f = java.util.concurrent.CompletableFuture.supplyAsync(() =>
      merged.toLazyList.take(6).toList)
    // flushes at 50ms; ten seconds is margin, not a measurement
    val got = f.get(10, java.util.concurrent.TimeUnit.SECONDS)
    assertEquals(got.sorted, List(1, 1, 2, 2, 3, 3))
  }

  test("merge(chunked, flushAfter) still chunks: a full source is not degraded to singletons") {
    // 40 elements a side at chunk 16 and a flush far longer than the
    // run: the timer must not turn this into the elementwise path
    val out = Source.of((1 to 40).toList)
      .merge(Source.of((41 to 80).toList), chunked = true, flushAfter = Some(30000))
      .toLazyList.toList
    assertEquals(out.toSet, (1 to 80).toSet)
  }

  test("Flush.now emits a partial chunk at the producer's own boundary, with no timer at all") {
    type R = Flush + (Writer % Int + Async)
    // three elements (fewer than a chunk), an explicit boundary, then
    // a source that never ends: only the Flush can get them out
    def marked: Flushing[Int] =
      okay.effect[R, Unit](Writer(1))
        .flatMap(_ => okay.effect[R, Unit](Writer(2)))
        .flatMap(_ => okay.effect[R, Unit](Writer(3)))
        .flatMap(_ => Flush.now[Writer % Int + Async])
        .flatMap(_ => okay.effect[R, Unit](Async.Run(() => Thread.sleep(60000))))

    // no flushAfter: the boundary is the ONLY thing that can emit
    val f = java.util.concurrent.CompletableFuture.supplyAsync(() =>
      marked.mergeFlushing(marked).toLazyList.take(6).toList)
    val got = f.get(10, java.util.concurrent.TimeUnit.SECONDS)
    assertEquals(got.sorted, List(1, 1, 2, 2, 3, 3))
  }

  test("Flush.now is a boundary, not a fence: full chunks still form around it") {
    type R = Flush + (Writer % Int + Async)
    // 20 elements, a boundary after the 18th: the first 16 leave as a
    // full chunk, the boundary sends 2, the rest flush at the end
    def many: Flushing[Int] =
      (1 to 20).foldLeft(okay.pure[R, Unit](())): (m, i) =>
        m.flatMap(_ => okay.effect[R, Unit](Writer(i)))
          .flatMap(_ => if i == 18 then Flush.now[Writer % Int + Async] else okay.pure(()))
    val out = many.mergeFlushing(okay.pure[Flush + (Writer % Int + Async), Unit](()))
      .toLazyList.toList
    assertEquals(out, (1 to 20).toList)
  }

  test("either: merge's own union, but each element still says which side it came from") {
    val a: Source[Int] = Source.of((1 to 50).toList)
    val b: Source[String] = Source.of((51 to 100).map(_.toString).toList)
    val out = a.either(b).toLazyList.toList
    assertEquals(out.length, 100)
    // no element crosses sides, and each side's own order survives —
    // the same promise `merge` makes, now checkable without a type test
    assertEquals(out.collect { case Left(i) => i }, (1 to 50).toList)
    assertEquals(out.collect { case Right(s) => s.toInt }, (51 to 100).toList)
  }

  test("either(chunked): the same tagging under the chunked merge") {
    val a: Source[Int] = Source.of((1 to 50).toList)
    val b: Source[String] = Source.of((51 to 100).map(_.toString).toList)
    val out = a.either(b, chunked = true).toLazyList.toList
    assertEquals(out.collect { case Left(i) => i }, (1 to 50).toList)
    assertEquals(out.collect { case Right(s) => s.toInt }, (51 to 100).toList)
  }

  test("Chunks.either: the chunked-stream merge, tagged the same way") {
    val a = Chunks.range(0, 50)
    val b = Chunks.map(Chunks.range(0, 50))(x => (x + 1000).toString)
    val merged = a.either(b)
    var out = Vector.empty[Either[Long, String]]
    var c = merged.receiveBlocking()
    while c.isDefined do { out ++= c.get; c = merged.receiveBlocking() }
    assertEquals(out.collect { case Left(i) => i }.sorted, (0L until 50L).toVector)
    assertEquals(out.collect { case Right(s) => s.toInt }.sorted, (1000 until 1050).toVector)
  }

  test("eitherFlushing: mergeFlushing's boundaries, elements tagged by side") {
    type R = Flush + (Writer % Int + Async)
    def marked(base: Int): Flushing[Int] =
      okay.effect[R, Unit](Writer(base + 1))
        .flatMap(_ => okay.effect[R, Unit](Writer(base + 2)))
        .flatMap(_ => okay.effect[R, Unit](Writer(base + 3)))
        .flatMap(_ => Flush.now[Writer % Int + Async])
        .flatMap(_ => okay.effect[R, Unit](Async.Run(() => Thread.sleep(60000))))

    val f = java.util.concurrent.CompletableFuture.supplyAsync(() =>
      marked(0).eitherFlushing(marked(100)).toLazyList.take(6).toList)
    val got = f.get(10, java.util.concurrent.TimeUnit.SECONDS)
    assertEquals(got.collect { case Left(i) => i }.sorted, List(1, 2, 3))
    assertEquals(got.collect { case Right(i) => i }.sorted, List(101, 102, 103))
  }
}
