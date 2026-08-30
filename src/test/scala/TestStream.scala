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
      effect[F, String](Writer("a")).flatMap: _ =>
        effect[F, Unit](Async.Run(() => Thread.sleep(1))).flatMap: _ =>
          effect[F, String](Writer("b")).map(_ => 7)
    assertEquals(talk.toLazyList.toList, List("a", "b"))
  }

  test("arbitrary effects: a State handler is a stream transformer") {
    type F = State % Int + (Writer % String + Async)
    def emits(n: Int): Unit ! F =
      if n == 0 then pure(())
      else effect[F, Int](State.Get()).flatMap: s =>
        effect[F, String](Writer(s"n$n+$s")).flatMap: _ =>
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

  test("a writer program joins through toLazyList") {
    def count(n: Int): Nothing ! Writer % Int =
      Writer.tell(n).flatMap(_ => count(n + 1))
    assertEquals(count(0).toLazyList.zip(nats[Int, Producer]).take(3).forall(_ == _), true)
  }
}
