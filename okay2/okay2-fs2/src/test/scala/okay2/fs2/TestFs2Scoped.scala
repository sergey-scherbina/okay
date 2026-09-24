package okay2.fs2

import _root_.fs2.Stream
import _root_.cats.effect.IO
import _root_.cats.effect.unsafe.implicits.global
import okay2._
import okay2.cats.Io
import Fs2Interop.{toFs2, fromFs2}

/** fromFs2 as a scoped pull under Resource (okay2-interop-async) */
class TestFs2Scoped extends munit.FunSuite {

  /** handle the tells, then the scope, then fold into IO */
  def collect[W](p: Unit ! (Writer[W] + Io + Resource)): Seq[W] =
    Io.run(Resource.run[(Seq[W], Unit), Io](Writer.run[W, Unit, Io + Resource](p))).unsafeRunSync()._1

  test("an IO stream is a Writer program, nothing pulled before it runs") {
    var pulled = 0
    val s: Stream[IO, Int] = Stream.range(1, 6).evalMap(i => IO { pulled += 1; i })
    val p = fromFs2(s)
    assertEquals(pulled, 0)
    assertEquals(collect(p), Seq(1, 2, 3, 4, 5))
    assertEquals(pulled, 5)
  }

  test("stopping early ENDS the stream: its finalizer runs when the scope ends, and the queue bounds how far it ran ahead") {
    @volatile var released = false
    @volatile var pulled = 0
    val s: Stream[IO, Int] =
      Stream.bracket(IO.unit)(_ => IO { released = true })
        .flatMap(_ => Stream.range(1, 1000000).chunkLimit(1).unchunks.evalMap(i => IO { pulled += 1; i }))
    val first3: Vector[Int] ! (Io + Resource) =
      Writer.foldUntil[Int, Vector[Int], Unit, Vector[Int], Io + Resource](fromFs2(s, capacity = 2))(FoldUntil.take(3))
    val got = Io.run(Resource.run[Vector[Int], Io](first3)).unsafeRunSync()
    assertEquals(got, Vector(1, 2, 3))
    assert(released, "the stream's finalizer did not run when the scope ended")
    assert(pulled < 100, s"the stream ran $pulled elements ahead of a consumer that took 3")
  }

  test("a failing stream fails the program, and its finalizer runs") {
    @volatile var released = false
    val s: Stream[IO, Int] =
      Stream.bracket(IO.unit)(_ => IO { released = true }).flatMap(_ => Stream(1, 2) ++ Stream.raiseError[IO](new RuntimeException("boom")))
    val e = intercept[RuntimeException](collect(fromFs2(s)))
    assertEquals(e.getMessage, "boom")
    assert(released)
  }

  test("round trip: stream -> scoped program -> stream, the scope handled by Failing.both over the whole row") {
    val s: Stream[IO, Int] = Stream.emits(List(3, 1, 2))
    val scoped: Unit ! (Writer[Int] + Io) =
      Resource.run[Unit, Writer[Int] + Io](fromFs2(s))(Failing.both[Writer[Int], Io])
    assertEquals(toFs2[IO, Int, Unit, Io](scoped).compile.toList.unsafeRunSync(), List(3, 1, 2))
  }
}
