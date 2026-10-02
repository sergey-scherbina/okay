package okay.zio

import _root_.zio.{Promise, Runtime, Unsafe, ZIO, durationInt}
import _root_.zio.stream.ZStream

/**
 * okay's class ladder over ZIO's types (specs/interop-classes.md): the
 * stream monad, and the parallel applicative chosen at the call site.
 */
class TestZioClasses extends munit.FunSuite with okay.testkit.Munit.Diagnosed {

  private def run[E, A](z: ZIO[Any, E, A]): A =
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  test("okay.traverse over ZStream is the cartesian product (the list monad's reading)") {
    val s = okay.traverse(Seq(1, 2))(i => ZStream(i, i * 10))
    assertEquals(run(s.runCollect).toList, List(Seq(1, 2), Seq(1, 20), Seq(10, 2), Seq(10, 20)))
  }

  test("okay.traverse over ZIO with the monad runs in order") {
    val out = run(okay.traverse(Seq(1, 2, 3))(i => ZIO.succeed(i * 2)))
    assertEquals(out, Seq(2, 4, 6))
  }

  test("parApplicative: two leaves that must meet, meet — a rendezvous, not a clock") {
    // each leaf completes its own promise, then waits for the other's:
    // in sequence the first waits for a leaf not started yet and times out
    val prog = for
      a <- Promise.make[Nothing, Unit]
      b <- Promise.make[Nothing, Unit]
      leaf = (mine: Promise[Nothing, Unit], theirs: Promise[Nothing, Unit]) =>
        mine.succeed(()) *> theirs.await.timeout(10.seconds).map(_.isDefined)
      out <- okay.traverse(Seq((a, b), (b, a)))(leaf.tupled)(using ZioClasses.parApplicative)
    yield out
    assertEquals(run(prog), Seq(true, true))
  }

  test("the monad's traverse over the same leaves cannot meet — the control") {
    val prog = for
      a <- Promise.make[Nothing, Unit]
      b <- Promise.make[Nothing, Unit]
      leaf = (mine: Promise[Nothing, Unit], theirs: Promise[Nothing, Unit]) =>
        mine.succeed(()) *> theirs.await.timeout(200.millis).map(_.isDefined)
      out <- okay.traverse(Seq((a, b), (b, a)))(leaf.tupled)
    yield out
    val out = run(prog)
    note(s"sequential leaves answered $out")
    assertEquals(out.head, false)
  }
}
