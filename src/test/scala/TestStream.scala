package okay

import !.*

/** The stream combinators, uniformly over the carriers — a Producer,
 * a writer program (via toLazyList), a LazyList — all lazy. */
class TestStream extends munit.FunSuite {

  def p12: Producer[Int] = produce(1).flatMap(_ => produce(2))

  test("map and flatMap (explicit: the monad owns the postfix names)") {
    assertEquals(Stream.map(nats[Int, Producer])(_ * 2).take(3).toList, List(0, 2, 4))
    assertEquals(Stream.flatMap(p12)(x => LazyList(x, x * 10)).toList, List(1, 10, 2, 20))
  }

  test("filter, collect, take, drop on an infinite producer") {
    assertEquals(nats[Int, Producer].filter(_ % 2 == 1).take(3).toList, List(1, 3, 5))
    assertEquals(nats[Int, Producer].collect { case x if x > 2 => -x }.take(2).toList, List(-3, -4))
    assertEquals(nats[Int, Producer].drop(3).take(2).toList, List(3, 4))
  }

  test("takeWhile and dropWhile") {
    assertEquals(nats[Int, Producer].takeWhile(_ < 4).toList, List(0, 1, 2, 3))
    assertEquals(nats[Int, Producer].dropWhile(_ < 4).take(2).toList, List(4, 5))
  }

  test("zip pairs streams across carriers, until either ends") {
    assertEquals(
      fibs[Long, Producer].zip(fibs[Long, LazyList]).take(4).toList,
      List((0L, 0L), (1L, 1L), (1L, 1L), (2L, 2L)))
    assertEquals(p12.zip(nats[Int, LazyList]).toList, List((1, 0), (2, 1)))
    assertEquals(nats[Int, Producer].zipWithIndex.take(3).forall(_ == _), true)
  }

  test("++ concatenates across carriers") {
    assertEquals((p12 ++ LazyList(3, 4)).toList, List(1, 2, 3, 4))
  }

  test("folds: the Fold algebra, and foldLeft") {
    assertEquals(Stream.fold(p12)(using summon[Fold[Int, Seq[Int]]]), Seq(1, 2))
    given Fold[Int, Int] = new:
      def init: Int = 0
      def add(s: Int, a: Int): Int = s + a
    assertEquals(Stream.fold[Producer, okay.Pure, Int, Int](p12), 3)
    assertEquals(nats[Int, Producer].take(5).foldLeft(0)(_ + _), 10)
  }

  test("search stops early, even on an infinite stream") {
    assertEquals(nats[Int, Producer].exists(_ > 5), true)
    assertEquals(nats[Int, Producer].forall(_ < 3), false)
    assertEquals(nats[Int, Producer].find(_ % 7 == 6), Some(6))
    assertEquals(nats[Int, Producer].headOption, Some(0))
  }

  test("instances: MonadPlus for LazyList and for Choice programs") {
    val MP = summon[MonadPlus[LazyList]]
    assertEquals(MP.empty[Int].append(LazyList(1)).append(LazyList(2)).toList, List(1, 2))
    val CP = summon[MonadPlus[[A] =>> A ! Choose]]
    assertEquals(!.run(runChoice[Int, Nothing](
      CP.append(choose(1, 2))(CP.empty).append(choose(3)))), Seq(1, 2, 3))
  }

  test("Monoid: numbers, strings, alternatives — and every Monoid folds") {
    assertEquals(1 |+| 2, 3)
    assertEquals("a" |+| "b", "ab")
    assertEquals((LazyList(1) |+| LazyList(2)).toList, List(1, 2))
    assertEquals(Stream.fold[Producer, okay.Pure, Int, Int](p12), 3)          // Fold from Monoid[Int]
    val words = produce("ab").flatMap(_ => produce("c"))
    assertEquals(Stream.fold[Producer, okay.Pure, String, String](words), "abc")
  }

  test("instances: Fold primitives and Foldable[Producer]") {
    assertEquals(Stream.fold(p12)(using Fold.sum[Int]), 3)
    assertEquals(Stream.fold(p12)(using Fold.count[Int]), 2L)
    assertEquals(Stream.fold(p12)(using Fold.first[Int]), Some(1))
    assertEquals(Stream.fold(p12)(using Fold.last[Int]), Some(2))
    assertEquals(p12.foldTo[Seq[Int]], Seq(1, 2))
  }

  test("instances: Monad for the Eff encoding") {
    val E = summon[Effects[Eff]]
    val M = summon[Monad[[A] =>> Eff[Produce, A]]]
    val m = M.flatMap(E.perform[Produce, Int](20))(x => M.pure(x + 22))
    assertEquals(m.runWith, 42)
  }

  test("arbitrary effects on a writer stream: async at the pull") {
    type F = Writer % String + Async
    val talk: Int ! F =
      effect[F, Unit](Writer("a")).flatMap: _ =>
        effect[F, Unit](Async.Run(() => Thread.sleep(1))).flatMap: _ =>
          effect[F, Unit](Writer("b")).map(_ => 7)
    assertEquals(talk.toLazyList.toList, List("a", "b"))
  }

  test("arbitrary effects: a State handler is a stream transformer") {
    type F = State % Int + (Writer % String + Async)
    def emits(n: Int): Unit ! F =
      if n == 0 then pure(())
      else effect[F, Int](State.Get()).flatMap: s =>
        effect[F, Unit](Writer(s"n$n+$s")).flatMap: _ =>
          effect[F, Int](State.Set(s + n)).flatMap(_ => emits(n - 1))
    val residue: (Int, Unit) ! (Writer % String + Async) =
      State.handle[Int, Unit, Writer % String + Async](0)(emits(3))
    assertEquals(residue.toLazyList.toList, List("n3+0", "n2+3", "n1+5"))
  }

  test("the triangle closes: generate materializes into the Teller too") {
    assertEquals(fibs[Long, Teller].toLazyList.take(10).toList,
      fibs[Long, LazyList].take(10).toList)
    assertEquals(nats[Int, Teller].toLazyList.take(5).toList, List(0, 1, 2, 3, 4))
  }

  test("Writer.of: any stream as a program — the direction back") {
    // a strict carrier, a lazy one, and an infinite one: the program
    // is pulled element by element, so the infinite source is fine
    assertEquals(Writer.of(List(1, 2, 3)).toLazyList.toList, List(1, 2, 3))
    assertEquals(Writer.of(LazyList(1, 2)).toLazyList.toList, List(1, 2))
    assertEquals(Writer.of(nats[Int, Producer]).toLazyList.take(3).toList, List(0, 1, 2))
  }

  test("Writer.map: the told values map, the effects stay where they were") {
    type F = Writer % String + Async
    val ran = collection.mutable.ListBuffer[String]()
    val talk: Int ! F =
      effect[F, Unit](Writer("a")).flatMap: _ =>
        effect[F, Unit](Async.Run(() => { ran += "effect"; () })).flatMap: _ =>
          effect[F, Unit](Writer("b")).map(_ => 7)
    val loud = Writer.map[String, String, Int, Async](talk)(_.toUpperCase)
    assertEquals(loud.toLazyList.toList, List("A", "B"))
    // the forwarded operation ran once, between the two tells
    assertEquals(ran.toList, List("effect"))
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
    // and each source's own order is preserved within the merge
    assertEquals(chunked.collect { case i: Int => i }, (1 to 50).toList)
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

  test("a writer program joins through toLazyList") {
    def count(n: Int): Nothing ! Writer % Int =
      Writer.tell(n).flatMap(_ => count(n + 1))
    assertEquals(count(0).toLazyList.zip(nats[Int, Producer]).take(3).forall(_ == _), true)
  }
}
