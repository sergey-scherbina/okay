package okay.freer

import okay.*
import okay.given


/**
 * CATCH FRAMES (handle-frames-catch, specs/handle-frames.md): a `try` over a program as DATA on the machine's
 * stack, so a `try` nested a hundred thousand deep holds no host `try` per level; and the frame catches what the
 * fold catches — a throw from the guarded program's own steps, not from an outer handler's clause.
 */
class TestHandleFramesCatch extends munit.FunSuite:

  final class Boom(val n: Int) extends RuntimeException(s"boom $n")

  def tryP[A](fa: => A ! Pure)(h: Throwable => A ! Pure): A ! Pure = summon[CanTry[[X] =>> X ! Pure]].tryIn(fa)(h)
  def tryD[A](fa: => A ! Shift % ? + Pure)(h: Throwable => A ! Shift % ? + Pure): A ! Shift % ? + Pure =
    summon[CanTry[[X] =>> X ! Shift % ? + Pure]].tryIn(fa)(h)

  test("a hundred thousand nested tries") {
    def nest(n: Int): Int ! Pure =
      if n == 0 then pure(0) else tryP(!.tailcall(nest(n - 1)).map(_ + 1))(_ => pure(-1))
    assertEquals(!.run(nest(100000)), 100000)
  }

  test("a hundred thousand nested tries, the innermost throws: the nearest catches") {
    def nest(n: Int): Int ! Pure =
      if n == 0 then pure(()).map(_ => throw Boom(0))
      else tryP(!.tailcall(nest(n - 1)).map(_ + 1))(_ => pure(if n == 1 then 1000 else -1))
    assertEquals(!.run(nest(100000)), 1000 + 99999)
  }

  /** a throw from the guarded program's continuation: caught, on the fold and on the machine alike */
  def throwing: Int ! Shift % ? + Pure = pure[Shift % ? + Pure, Int](1).map(x => if x == 1 then throw Boom(1) else x)

  test("a throw from a continuation step: the fold catches it") {
    assertEquals(!.run(tryP(pure[Pure, Int](1).map(x => if x == 1 then throw Boom(1) else x))(_ => pure(42))), 42)
  }

  test("a throw from a continuation step: the frame catches it, on the machine that runs the try") {
    assertEquals(!.run(Shift.run[Int, Pure](tryD(throwing)(_ => pure(42)))), 42)
  }

  test("a capture through the try and back: the throw after the resumption is the frame's") {
    val p = Shift.prompt[Int]
    val prog: Int ! Shift % ? + Pure = Shift.push[Int, Pure](p)(
      tryD(Shift.shift[Int, Int, Pure](p)(k => k(1).map(_ * 10)).map(x => if x == 1 then throw Boom(2) else x))(_ => pure(7)))
    // k(1) re-enters the try, the throw lands in the frame: 7, then * 10
    assertEquals(!.run(Shift.run[Int, Pure](prog)), 70)
  }

  test("a throw no frame takes goes on, the same object") {
    val b = Boom(9)
    val got = intercept[Boom](!.run(Shift.run[Int, Pure](pure[Shift % ? + Pure, Int](1).map(_ => throw b))))
    assert(got eq b)
  }

  test("a handler that declines (rethrows) passes the throw to the try outside it") {
    val inner = tryD(throwing)(t => throw t)
    assertEquals(!.run(Shift.run[Int, Pure](tryD(inner)(_ => pure(5)))), 5)
  }
