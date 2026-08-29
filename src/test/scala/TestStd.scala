package okay

import !.*

/** The standard effects (Reader, Writer, Choice), the Fold/Foldable
 * consumers, the Stream codata, and the streaming bridges. */
class TestStd extends munit.FunSuite {

  test("Reader: every ask answers the environment, other effects forwarded") {
    type F = Reader % Int + Produce
    val prog: Int ! F =
      effect[F, Int](Reader.Ask()).flatMap: x =>
        effect[F, Int](10).flatMap: y =>
          effect[F, Int](Reader.Ask()).map(z => x + y + z)
    assertEquals(Reader.run[Int, Int, Produce](21)(prog).runWith, 52)
  }

  test("Writer: run collects everything told, in order, forwarding F") {
    type F = Writer % String + Produce
    val prog: Int ! F =
      effect[F, String](Writer("a")).flatMap: _ =>
        effect[F, Int](1).flatMap: x =>
          effect[F, String](Writer("b")).map(_ => x + 1)
    val (ws, a) = Writer.run[String, Int, Produce](prog).runWith
    assertEquals(ws, Seq("a", "b"))
    assertEquals(a, 2)
  }

  test("Writer: fold into a custom Fold (a sum instead of a Seq)") {
    given Fold[Int, Long] = new:
      def init: Long = 0L
      def add(s: Long, w: Int): Long = s + w
    val w: String ! Writer % Int =
      Writer.tell(1).flatMap(_ => Writer.tell(2)).flatMap(_ => Writer.tell(3)).map(_ => "done")
    val (sum, a) = !.run(Writer.fold[Int, Long, String, Nothing](w))
    assertEquals(sum, 6L)
    assertEquals(a, "done")
  }

  test("Writer: uncons observes the told values one by one, the answer last") {
    val w: Int ! Writer % String =
      Writer.tell("a").flatMap(_ => Writer.tell("b")).map(_.length)
    val Right(("a", r1)) = Writer.uncons(w): @unchecked
    val Right(("b", r2)) = Writer.uncons(r1): @unchecked
    assertEquals(Writer.uncons(r2), Left(1))
  }

  test("Writer is a stream too: an infinite teller unfolds on demand") {
    def count(n: Int): Nothing ! Writer % Int =
      Writer.tell(n).flatMap(_ => count(n + 1))
    assertEquals(count(0).toLazyList.take(5).toList, List(0, 1, 2, 3, 4))
    assertEquals(count(0).uncons.map(_._1), Some(0))
  }

  test("Reader and Writer compose") {
    type F = Reader % Int + Writer % String
    val prog: Int ! F =
      effect[F, Int](Reader.Ask()).flatMap: x =>
        effect[F, String](Writer(s"got $x")).map(_ => x * 2)
    val (ws, a) = !.run(Writer.run[String, Int, Nothing](
      Reader.run[Int, Int, Writer % String](7)(prog)))
    assertEquals(ws, Seq("got 7"))
    assertEquals(a, 14)
  }

  test("Foldable: run one Fold over any container") {
    assertEquals(List(1, 2, 3).foldTo[Seq[Int]], Seq(1, 2, 3))
    given Fold[Int, Int] = new:
      def init: Int = 0
      def add(s: Int, a: Int): Int = s + a
    assertEquals(Iterator(1, 2, 3).foldTo[Int], 6)
  }

  test("Stream is codata: uncons observes, toLazyList is the anamorphism") {
    val Some((h, t)) = fibs[Long, LazyList].uncons: @unchecked
    assertEquals(h, 0L)
    assertEquals(t.uncons.map(_._1), Some(1L))
    assertEquals(LazyList.empty[Int].uncons, None)
    assertEquals(fibs[Long, LazyList].toLazyList.take(5).toList,
      List(0L, 1L, 1L, 2L, 3L))
  }

  test("Choice: multi-shot handler explores every branch (cartesian)") {
    val prog: Int ! Choose =
      choose(1, 2, 3).flatMap(x => choose(10, 20).map(x * _))
    assertEquals(!.run(runChoice[Int, Nothing](prog)),
      Seq(10, 20, 20, 40, 30, 60))
  }

  test("Choice: empty choice prunes the branch") {
    val prog: Int ! Choose =
      choose(1, 2).flatMap(x => if x == 1 then choose[Int]() else choose(x))
    assertEquals(!.run(runChoice[Int, Nothing](prog)), Seq(2))
  }

  test("a producer is a stream: uncons steps it, toLazyList unfolds it") {
    val Some((h, t)) = fibs[Long, Producer].uncons: @unchecked
    assertEquals(h, 0L)
    assertEquals(t.uncons.map(_._1), Some(1L))
    val Some((x, end)) = produce(42).uncons: @unchecked
    assertEquals(x, 42)
    assertEquals(end.uncons, None)
    assertEquals(
      fibs[Long, Producer].toLazyList.take(10).toList,
      fibs[Long, LazyList].take(10).toList)
    assertEquals(
      nats[Int, Producer].toLazyList.take(5).toList,
      List(0, 1, 2, 3, 4))
  }
}
