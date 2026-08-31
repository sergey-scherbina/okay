package okay.zio

import okay.{Async, Chunks, async, Fold}
import okay.given
import ZioInterop.*
import _root_.zio.{Runtime, Unsafe, ZIO}
import _root_.zio.stream.ZStream

class TestZioInterop extends munit.FunSuite {

  test("Async and ZIO bridge both ways") {
    val z = toZIO(async(40).map(_ + 2))
    val r = Unsafe.unsafe(implicit u =>
      Runtime.default.unsafe.run(z).getOrThrowFiberFailure())
    assertEquals(r, 42)
    assertEquals(fromZIO(ZIO.attempt(21).map(_ * 2)).runWith, 42)
  }

  test("chunked streams cross to ZStream chunk for chunk") {
    val z = toZStream(Chunks.range(0, 100, 16))
    val out = Unsafe.unsafe(implicit u =>
      Runtime.default.unsafe.run(z.runCollect).getOrThrowFiberFailure())
    assertEquals(out.toList, (0L until 100L).toList)
    // an infinite okay stream stays lazy on the zio side
    val heads = Unsafe.unsafe(implicit u =>
      Runtime.default.unsafe.run(toZStream(Chunks.nats[Int]()).take(5).runCollect)
        .getOrThrowFiberFailure())
    assertEquals(heads.toList, List(0, 1, 2, 3, 4))
  }

  test("our Scheduler runs on the ZIO runtime") {
    given okay.Scheduler = ZioInterop.scheduler()
    val f = okay.Async.spawn(okay.async { Thread.sleep(10); 21 })
    assertEquals(f.join() * 2, 42)
    assertEquals(okay.Async.par(okay.async(1), okay.async(2)).runWith, (1, 2))
  }

  test("ZStream crosses back and is consumed lazily") {
    var built = 0
    val zs = ZStream.iterate(0)(_ + 1).tap(_ => ZIO.succeed { built += 1 })
    val c = fromZStream(zs.take(200), 16)
    assertEquals(Chunks.fold(Chunks.take(c)(32))(using Fold.count), 32L)
    assert(built <= 64, s"eagerly built $built")   // a chunk or two, not 200
  }
}
