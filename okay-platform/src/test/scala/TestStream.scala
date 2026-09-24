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

  test("the linear consumers agree with the LazyList bridge, on a Producer and on a Feed") {
    // stream-fold-via-iterator: foldLeft/foreach/toList/Stream.fold walk
    // `iterator`; the bridge is the oracle they must agree with
    val feed: Feed[Int] = Writer.tell(1).flatMap(_ => Writer.tell(2)).flatMap(_ => Writer.tell(3))
    val St = feedStream[Unit]
    assertEquals(St.iterator(feed).toList, feed.toLazyList.toList)
    assertEquals(p12.toList, p12.toLazyList.toList)
    assertEquals(p12.foldLeft(List.empty[Int])((l, a) => a :: l), p12.toLazyList.foldLeft(List.empty[Int])((l, a) => a :: l))
    val seen = collection.mutable.ListBuffer.empty[Int]
    p12.foreach(seen += _)
    assertEquals(seen.toList, List(1, 2))
    assertEquals(Stream.fold[[W] =>> Unit ! Writer % W, okay.Pure, Int, Int](feed)(using Fold.sum[Int]), 6)
    assertEquals(Stream.fold[[W] =>> Unit ! Writer % W, okay.Pure, Int, Long](feed)(using Fold.count[Int]), 3L)
    assertEquals(Stream.fold(okay.pure[Produce, Int](0): Producer[Int])(using Fold.count[Int]), 0L)
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
    val residue: (Int, Unit) ! Writer % String + Async =
      State.handle[Int](0)(emits(3))
    assertEquals(residue.toLazyList.toList, List("n3+0", "n2+3", "n1+5"))
  }

  test("the fourth carrier closes: generate materializes into the writer stream too") {
    type Told[W] = Unit ! Writer % W
    assertEquals(fibs[Long, Told].toLazyList.take(10).toList,
      fibs[Long, LazyList].take(10).toList)
    assertEquals(nats[Int, Told].toLazyList.take(5).toList, List(0, 1, 2, 3, 4))
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

  test("Writer.expand: one told value becomes many, none, or one") {
    type F = Writer % String + Async
    val ran = collection.mutable.ListBuffer[String]()
    val talk: Int ! F =
      effect[F, Unit](Writer("ab")).flatMap: _ =>
        effect[F, Unit](Async.Run(() => { ran += "effect"; () })).flatMap: _ =>
          effect[F, Unit](Writer("")).flatMap: _ =>
            effect[F, Unit](Writer("cd")).map(_ => 7)

    // each told string becomes its characters: "ab" -> a, b; "" -> nothing
    val split = Writer.expand[String, Char, Int, Async](talk)(_.toVector)
    assertEquals(split.toLazyList.toList, List('a', 'b', 'c', 'd'))
    // the forwarded operation ran once, in its own place in the order
    assertEquals(ran.toList, List("effect"))

    // one-for-one agrees with `map`, which is expand's degenerate case
    val loud = Writer.expand[String, String, Int, Async](talk)(w => Vector(w.toUpperCase))
    assertEquals(loud.toLazyList.toList,
      Writer.map[String, String, Int, Async](talk)(_.toUpperCase).toLazyList.toList)
  }

  test("a writer program joins through toLazyList") {
    def count(n: Int): Nothing ! Writer % Int =
      Writer.tell(n).flatMap(_ => count(n + 1))
    assertEquals(count(0).toLazyList.zip(nats[Int, Producer]).take(3).forall(_ == _), true)
  }
}
