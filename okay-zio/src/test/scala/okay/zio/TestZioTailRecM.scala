package okay.zio

import _root_.zio.{Runtime, Unsafe, ZIO}
import _root_.zio.stream.ZStream

/** ZIO's and ZStream's loops (specs/eager-carrier-depth.md): their
 * `flatMap` defers, so `TailRecM.deferring` is their loop — checked by
 * BUILDING a million-step loop on a 128 KB thread (a carrier that called
 * its continuation at once would overflow there at a thousand) and
 * running it on ZIO's own fibers */
class TestZioTailRecM extends munit.FunSuite with okay.testkit.Munit.Diagnosed {

  private def onSmallStack[A](body: => A): A =
    var out: Either[Throwable, A] = Left(IllegalStateException("never ran"))
    val t = Thread(null, () => out = try Right(body) catch case e: Throwable => Left(e), "small-stack", 128L * 1024)
    t.start(); t.join()
    out.fold(e => throw e, identity)

  private def run[E, A](z: ZIO[Any, E, A]): A =
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  val n = 1000000

  test("ZIO: a million iterations, built on a 128 KB thread") {
    val R = summon[okay.TailRecM[[A] =>> ZIO[Any, Nothing, A]]]
    val z = onSmallStack(R.tailRecM(0)(i => ZIO.succeed(if i < n then Left(i + 1) else Right(i))))
    assertEquals(run(z), n)
  }

  test("ZStream: a hundred thousand iterations, built on a 128 KB thread") {
    val R = summon[okay.TailRecM[[A] =>> ZStream[Any, Nothing, A]]]
    val s = onSmallStack(R.tailRecM(0)(i => ZStream.succeed(if i < 100000 then Left(i + 1) else Right(i))))
    assertEquals(run(s.runCollect).toList, List(100000))
  }
}
