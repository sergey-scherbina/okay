package okay.zio

import okay.{Async}

import okay.freer.{%, +}
import okay.freer.{!}
import okay.std.{Reader, Throws, raise, runEither}
import okay.freer.Row.at
import okay.given
import okay.freer.given
import okay.std.given
import ZioInterop.*
import _root_.zio.{Exit, Runtime, Unsafe, ZEnvironment, ZIO}
import java.util.concurrent.{CountDownLatch, TimeUnit}

/** specs/zio-typed-row.md: ZIO[R, E, A] <-> A ! Reader % ZEnvironment[R] + Throws % E + Async */
class TestZioTypedRow extends munit.FunSuite:

  final case class Greeting(word: String)
  type Row = Reader % ZEnvironment[Greeting] + Throws % String + Async

  private val hi = ZEnvironment(Greeting("hi"))
  private val yo = ZEnvironment(Greeting("yo"))

  /** the okay side's three handlers: Reader, then Throws, then Async */
  private def handled[A](env: ZEnvironment[Greeting], p: A ! Row): Either[String, A] ! Async =
    runEither[A, Async, String](Reader.run[ZEnvironment[Greeting], A, Throws % String + Async](env)(p))

  private def exit[E, A](z: ZIO[Any, E, A]): Exit[E, A] =
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z))

  test("fromZIOTyped reads the environment from the Reader") {
    val z: ZIO[Greeting, String, String] = ZIO.serviceWith[Greeting](_.word + "!")
    assertEquals(handled(hi, fromZIOTyped(z)).runWith, Right("hi!"))
    assertEquals(handled(yo, fromZIOTyped(z)).runWith, Right("yo!"))
  }

  test("a typed ZIO failure is raise on the okay side") {
    val z: ZIO[Greeting, String, Int] = ZIO.fail("nope")
    assertEquals(handled(hi, fromZIOTyped(z)).runWith, Left("nope"))
  }

  test("a ZIO defect fails the Async run, not the Throws") {
    val boom = RuntimeException("boom")
    val z: ZIO[Greeting, String, Int] = ZIO.die(boom)
    assertEquals(intercept[RuntimeException](handled(hi, fromZIOTyped(z)).runWith), boom)
  }

  test("toZIOTyped: the Reader is ZIO's environment, raise is fail") {
    val p: String ! Row = Reader.ask[ZEnvironment[Greeting]].at[Row].map(_.get[Greeting].word)
    assertEquals(exit(toZIOTyped(p).provideEnvironment(hi)), Exit.succeed("hi"))
    val q: Int ! Row = raise[String, Int]("bad").at[Row]
    assertEquals(exit(toZIOTyped(q).provideEnvironment(hi)), Exit.fail("bad"))
  }

  test("a throwable escaping the okay program is a ZIO defect") {
    val boom = RuntimeException("boom")
    val p: Int ! Row = okay.async[Int](throw boom).at[Row]
    exit(toZIOTyped(p).provideEnvironment(hi)) match
      case Exit.Failure(c) =>
        assertEquals(c.dieOption, Some(boom))
        assertEquals(c.failureOption, None)
      case other => fail(s"expected a defect, got $other")
  }

  test("round trip answers as the ZIO itself") {
    val zs: List[ZIO[Greeting, String, String]] = List(
      ZIO.succeed("plain"), ZIO.fail("typed"), ZIO.serviceWith[Greeting](_.word))
    for z <- zs do
      assertEquals(exit(toZIOTyped(fromZIOTyped(z)).provideEnvironment(yo)), exit(z.provideEnvironment(yo)))
  }

  test("cancelling the okay side interrupts the typed ZIO") {
    val started = CountDownLatch(1)
    val finalized = CountDownLatch(1)
    val z: ZIO[Greeting, String, Int] =
      (ZIO.succeed(started.countDown()) *> ZIO.never).onInterrupt(ZIO.succeed(finalized.countDown()))
    val running = Async.runAsyncCancellable(handled(hi, fromZIOTyped(z)))
    assert(started.await(5, TimeUnit.SECONDS))
    running.cancel()
    assert(finalized.await(5, TimeUnit.SECONDS), "the ZIO finalizer ran")
  }
