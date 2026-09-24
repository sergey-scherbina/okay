package okay2.fs2

import _root_.fs2.Stream
import _root_.cats.effect.IO
import _root_.cats.effect.unsafe.implicits.global
import okay2._
import okay2.Produce.produce
import okay2.cats.Io
import okay2.cats.CatsInterop.Into
import Fs2Interop.toFs2

class TestFs2Interop extends munit.FunSuite {

  test("a Writer program is a pure fs2 stream") {
    val p: Unit ! Writer[Int] = (1 to 5).foldLeft(pure[Writer[Int], Unit](()))((m, i) => m.flatMap(_ => Writer.tell(i)))
    val s: Stream[_root_.fs2.Pure, Int] = toFs2[_root_.fs2.Pure, Int, Unit, Pure](p)
    assertEquals(s.toList, List(1, 2, 3, 4, 5))
  }

  test("a Writer program beside IO is an IO stream, the IO run between the elements") {
    type Row = Writer[String] + Io
    var side = List.empty[String]
    val p: Unit ! Row = for {
      _ <- Writer.tell("a").at[Row]
      _ <- Io.lift(IO { side ::= "io" }).at[Row]
      _ <- Writer.tell("b").at[Row]
    } yield ()
    val s: Stream[IO, String] = toFs2[IO, String, Unit, Io](p)
    assertEquals(side, Nil) // building the stream ran nothing
    assertEquals(s.compile.toList.unsafeRunSync(), List("a", "b"))
    assertEquals(side, List("io"))
    // taking one element runs nothing past it
    side = Nil
    assertEquals(s.take(1).compile.toList.unsafeRunSync(), List("a"))
    assertEquals(side, Nil)
  }

  test("another effect beside the Writer is run by its own Into") {
    type Row = Writer[Int] + Produce
    implicit val produceIO: Into[Produce, IO] = new Into.Of[Produce, IO] {
      def apply[X](e: Produce.Emit[X]): IO[X] = IO.pure(e.a)
    }
    val p: Unit ! Row = produce(2).at[Row].flatMap(n => Writer.tell(n).at[Row]).flatMap(_ => Writer.tell(3).at[Row])
    assertEquals(toFs2[IO, Int, Unit, Produce](p).compile.toList.unsafeRunSync(), List(2, 3))
  }

  test("stack safety: a million tells as a stream") {
    val n = 1000000
    val p: Unit ! Writer[Int] = (1 to n).foldLeft(pure[Writer[Int], Unit](()))((m, i) => m.flatMap(_ => Writer.tell(i)))
    assertEquals(toFs2[_root_.fs2.Pure, Int, Unit, Pure](p).fold(0L)(_ + _).toList, List(n.toLong * (n + 1) / 2))
  }
  // fromFs2 is scoped now: TestFs2Scoped
}
