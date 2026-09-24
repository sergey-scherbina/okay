package okay2.zio

import java.util.concurrent.atomic.AtomicBoolean

import _root_.zio.{Runtime, Task, Unsafe, ZIO}
import _root_.zio.stream.ZStream
import okay2._
import okay2.async.Async
import okay2.platform._
import ZioInterop.{fromZStream, fromZIO, toZIO, toZStream}

/** the Async bridge and the scoped ZStream source (okay2-interop-async) */
class TestZioAsync extends munit.FunSuite {

  private def run[A](z: Task[A]): A =
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  def collect[W](p: Unit ! (Writer[W] + Zio + Resource)): Seq[W] =
    run(Zio.run(Resource.run[(Seq[W], Unit), Zio](Writer.run[W, Unit, Zio + Resource](p))))._1

  test("toZIO runs an Async program as a blocking ZIO; fromZIO is a ZIO as an Async operation") {
    val p: Int ! Async = fromZIO(ZIO.succeed(20)).flatMap(a => Async(a + 22))
    assertEquals(run(toZIO(p)), 42)
    assertEquals(!.run(Async.run[Int, Pure](p)), 42)
  }

  test("fromZIO is interrupted when the waiting side gives up") {
    val interrupted = new AtomicBoolean(false)
    val never: Task[Unit] = ZIO.never.onInterrupt(ZIO.succeed(interrupted.set(true)))
    val p = Async.timeout(50)(fromZIO(never))
    assertEquals(!.run(Async.run[Option[Unit], Pure](p)), None)
    var i = 0
    while (!interrupted.get && i < 200) { Thread.sleep(10); i += 1 }
    assert(interrupted.get, "the ZIO kept running after the Async side timed out")
  }

  test("scheduler: okay2's par on the ZIO runtime") {
    implicit val S: okay2.async.Scheduler = ZioInterop.scheduler()
    val p = Async.par(Async(1), Async(2))
    assertEquals(!.run(Async.run[(Int, Int), Pure](p)), (1, 2))
  }

  test("a ZStream is a scoped Writer program: collected, chunk by chunk") {
    assertEquals(collect(fromZStream(ZStream(3, 1, 2))), Seq(3, 1, 2))
  }

  test("stopping early closes the stream's scope: its release runs when the program's scope ends") {
    val released = new AtomicBoolean(false)
    val s: ZStream[Any, Throwable, Int] =
      ZStream.acquireReleaseWith(ZIO.unit)(_ => ZIO.succeed(released.set(true))).flatMap(_ => ZStream.iterate(1)(_ + 1))
    val first3: Vector[Int] ! (Zio + Resource) =
      Writer.foldUntil[Int, Vector[Int], Unit, Vector[Int], Zio + Resource](fromZStream(s))(FoldUntil.take(3))
    assertEquals(run(Zio.run(Resource.run[Vector[Int], Zio](first3))), Vector(1, 2, 3))
    assert(released.get, "the stream's scope stayed open after the program's scope ended")
  }

  test("round trip through the whole row's scope") {
    val scoped: Unit ! (Writer[Int] + Zio) =
      Resource.run[Unit, Writer[Int] + Zio](fromZStream(ZStream(3, 1, 2)))(Failing.both[Writer[Int], Zio])
    assertEquals(run(toZStream[Any, Throwable, Int, Unit, Zio](scoped).runCollect).toList, List(3, 1, 2))
  }
}
