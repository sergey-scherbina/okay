package okay.interop

import okay.{!, Async, async, asOkay, >=>}
import okay.given
import okay.Direct.*
import okay.cats.asIO
import okay.cats.given
import okay.zio.asZIO
import okay.zio.given
import okay.kyo.asKyo
import okay.kyo.given
import _root_.cats.effect.IO
import _root_.cats.effect.unsafe.implicits.global
import _root_.zio.{Runtime, Task, Unsafe, ZIO}
import _root_.kyo.{<, Abort, AllowUnsafe, Duration, KyoApp}

/**
 * ONE EXPRESSION ACROSS LIBRARIES (specs/interop-compose.md): functions
 * written with cats, ZIO, kyo and okay composed in one chain and one
 * block, and okay functions called from inside each library's own code.
 * One file importing all three interops is itself a check: `asOkay` is
 * ONE extension (okay's), each library an instance of `ToOkay`.
 */
class TestMixed extends munit.FunSuite with okay.testkit.Munit.Diagnosed {

  // ---- one function from each library

  val parse: String => IO[Int] = s => IO(s.trim.toInt)                          // cats
  val double: Int => Task[Int] = i => ZIO.succeed(i * 2)                        // ZIO
  val inc: Int => Int < (Abort[Nothing] & _root_.kyo.Async) = i => _root_.kyo.IO(i + 1) // kyo
  val show: Int => String ! Async = i => async(s"<$i>")                         // okay

  private def runZ[A](z: Task[A]): A =
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  private def runK[A: _root_.kyo.Flat](k: A < (Abort[Nothing] & _root_.kyo.Async)): A =
    import AllowUnsafe.embrace.danger
    KyoApp.Unsafe.runAndBlock(Duration.Infinity)(k).getOrThrow

  test("Kleisli: cats >=> ZIO >=> kyo >=> okay, one chain") {
    val all: String => String ! Async = parse.asOkay >=> double.asOkay >=> inc.asOkay >=> show
    assertEquals(all(" 20 ").runWith, "<41>")
  }

  test("direct: an IO, a ZIO, a kyo value and an okay program in one block") {
    val p: String ! Async = direct {
      val a = IO(10).?
      val b = ZIO.succeed(10).?
      val c = inc(20).asOkay.?
      show(a + b + c).?
    }
    assertEquals(p.runWith, "<41>")
  }

  test("okay inside cats: an IO for-comprehension calls okay functions") {
    val io = for
      x <- parse("20")
      y <- show(x).asIO
      z <- IO(41).flatMap(show.asIO)
    yield y + z
    assertEquals(io.unsafeRunSync(), "<20><41>")
  }

  test("okay inside ZIO: a ZIO chain calls okay functions") {
    val z = for
      x <- double(10)
      y <- show(x).asZIO
      z <- ZIO.succeed(41).flatMap(show.asZIO)
    yield y + z
    assertEquals(runZ(z), "<20><41>")
  }

  test("okay inside kyo: a kyo chain calls okay functions") {
    val k: String < (Abort[Nothing] & _root_.kyo.Async) =
      for
        x <- inc(19)
        y <- show(x).asKyo
        z <- show.asKyo(41)
      yield y + z
    assertEquals(runK(k), "<20><41>")
  }

  test("round trip: okay calls cats, calling ZIO, calling kyo, calling okay") {
    // kyo's stage named, so its effect set is written down rather than
    // inferred through a nested `<`
    val viaKyo: Int => String < (Abort[Nothing] & _root_.kyo.Async) = n => inc(n).flatMap(show.asKyo)
    val deep: Int => String ! Async = i =>
      IO(i).flatMap(x => ZIO.succeed(x).flatMap(y => viaKyo(y).asOkay.asZIO).asOkay.asIO).asOkay
    note("six crossings in one expression")
    assertEquals(deep(40).runWith, "<41>")
  }
}
