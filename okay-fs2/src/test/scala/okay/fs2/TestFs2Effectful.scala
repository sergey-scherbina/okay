package okay.fs2

import okay.{Async, Source, Stage}

import okay.freer.{%, +}
import okay.freer.{!}
import okay.std.{Writer}
import okay.given
import okay.freer.given
import okay.std.given
import okay.cats.CatsEffect
import okay.cats.CatsEffect.Program
import _root_.cats.effect.IO
import _root_.cats.effect.unsafe.implicits.global
import _root_.fs2.Stream
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*

/**
 * Effectful okay streams in fs2 and back (specs/fs2-effectful.md).
 */
class TestFs2Effectful extends munit.FunSuite with okay.testkit.Munit.Diagnosed {

  /** an okay source: async steps between its tells */
  def counted(n: Int): Source[Int] =
    def go(i: Int): Source[Int] =
      if i > n then okay.freer.pure(())
      else !.widen[Int, Async, Writer % Int](okay.async(i))
        .flatMap(x => okay.freer.effect[Writer % Int + Async, Unit](Writer.Say(x)))
        .flatMap(_ => go(i + 1))
    go(1)

  def collect[A](s: Source[A]): Seq[A] = Writer.run[A, Unit, Async](s).runWith._1

  test("a Source with async steps is an fs2 IO stream, in order") {
    assertEquals(Fs2Streams.toFs2[IO, Int](counted(5)).compile.toList.unsafeRunSync(), List(1, 2, 3, 4, 5))
  }

  test("an interrupted fs2 stream cancels the Await its source was parked on") {
    val unregistered = AtomicInteger(0)
    val parked: Source[Int] =
      okay.freer.effect[Writer % Int + Async, Unit](Writer.Say(1))
        .flatMap(_ => !.widen[Unit, Async, Writer % Int](Async.await[Unit](_ => () => { unregistered.incrementAndGet(); () })))
    val out = Fs2Streams.toFs2[IO, Int](parked).interruptAfter(50.millis).compile.toList.unsafeRunSync()
    note(s"answered $out")
    assertEquals(out, List(1))
    assertEquals(unregistered.get, 1)
  }

  test("an fs2 IO stream is a Source, every chunk in order") {
    val s = Stream.emits(1 to 1000).covary[IO].chunkLimit(64).unchunks
    assertEquals(collect(Fs2Streams.fromFs2(s, capacity = 4)), (1 to 1000).toList)
  }

  test("cancelling the okay side cancels the fs2 fiber behind fromFs2") {
    val finalized = AtomicInteger(0)
    val endless = Stream.iterate(0)(_ + 1).covary[IO].onFinalize(IO { finalized.incrementAndGet(); () })
    val running = Async.runAsyncCancellable(Writer.run[Int, Unit, Async](Fs2Streams.fromFs2(endless, capacity = 2)))
    Thread.sleep(50)
    running.cancel()
    val deadline = System.nanoTime() + 5.seconds.toNanos
    while finalized.get == 0 && System.nanoTime() < deadline do Thread.sleep(5)
    note(s"finalized ${finalized.get}")
    assertEquals(finalized.get, 1)
  }

  /** a stage that keeps a running sum and stops after `n` inputs */
  def runningSum(n: Int): Stage[Int, Int, Unit] =
    def go(seen: Int, sum: Int): Stage[Int, Int, Unit] =
      if seen == n then okay.freer.pure(())
      else Stage.await[Int, Int].flatMap {
        case Some(i) => Stage.tell[Int, Int](sum + i).flatMap(_ => go(seen + 1, sum + i))
        case None => okay.freer.pure(())
      }
    go(0, 0)

  test("a Stage is a Pipe that pulls only what it asks for: an infinite input, three taken") {
    val out = Stream.iterate(1)(_ + 1).through(Fs2Streams.toPipe[_root_.fs2.Pure, Int, Int](runningSum(3))).toList
    assertEquals(out, List(1, 3, 6))
  }

  test("a Stage as a Pipe over 100 000 elements: the walk stays off the stack") {
    val n = 100000
    val out = Stream.range(0, n).through(Fs2Streams.toPipe[_root_.fs2.Pure, Int, Int](Stage.id[Int])).compile.count
    assertEquals(out, n.toLong)
  }

  test("an fs2 pipe in an okay pipeline: Source through a Pipe, back to a Source") {
    val out = collect(Fs2Streams.through(counted(6))(_.map(_ * 10).filter(_ > 20)))
    assertEquals(out, List(30, 40, 50, 60))
  }

  test("an fs2 stream compiled AT an okay program: evalMap and parEvalMap, no IO in its type") {
    val s: Stream[Program, Int] =
      Fs2Streams.toFs2[Program, Int](counted(8))
        .evalMap(i => CatsEffect.lift(okay.async(i * 2)))
        .parEvalMap(4)(i => CatsEffect.lift(okay.async(i + 1)))
    val p: Program[List[Int]] = s.compile.toList
    assertEquals(CatsEffect.toIO(p).unsafeRunSync(), (1 to 8).map(i => i * 2 + 1).toList)
  }
}
