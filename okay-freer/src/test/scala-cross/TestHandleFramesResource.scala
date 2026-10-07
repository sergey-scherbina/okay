package okay.freer

import okay.*
import okay.given

import okay.freer.Row.*

/**
 * Resource's scope as a FRAME (handle-frames-catch): nested a hundred thousand deep with no host `try` per level,
 * and on a machine it releases as the walk does — at the end, at a throw from inside (and the throw goes on), at a
 * final operation passing through, and each acquisition once.
 */
class TestHandleFramesResource extends munit.FunSuite:

  type D = Shift % ? + Pure
  given Failing[D] = new Failing[D]:
    def guard[X](e: D[X], onFailure: () => Unit): D[X] = e

  final class Boom extends RuntimeException("boom")

  test("a hundred thousand nested scopes: every acquisition released once, inner first") {
    val released = scala.collection.mutable.ArrayBuffer.empty[Int]
    def nest(n: Int): Int ! Pure =
      if n == 0 then pure(0)
      else Resource.run[Int, Pure](
        Resource.acquire(n)(r => released += r).at[Resource + Pure].flatMap(_ =>
          !.tailcall(nest(n - 1)).at[Resource + Pure].map(_ + 1)))
    assertEquals(!.run(nest(100000)), 100000)
    assertEquals(released.size, 100000)
    assertEquals(released.take(3).toList, List(1, 2, 3))
  }

  /** the scope run INSIDE a machine: `Shift.run` steps into it as a frame */
  def onMachine[A](p: A ! Resource + D): A = !.run(Shift.run[A, Pure](Resource.run[A, D](p)))

  test("on a machine: released at the end, in reverse") {
    val log = scala.collection.mutable.ArrayBuffer.empty[String]
    val r = onMachine(for
      a <- Resource.acquire("a")(x => log += s"release $x").at[Resource + D]
      b <- Resource.acquire("b")(x => log += s"release $x").at[Resource + D]
    yield a + b)
    assertEquals(r, "ab")
    assertEquals(log.toList, List("release b", "release a"))
  }

  test("on a machine: a throw from inside releases what is held, and goes on") {
    val log = scala.collection.mutable.ArrayBuffer.empty[String]
    val _ = intercept[Boom](onMachine(for
      a <- Resource.acquire("a")(x => log += s"release $x").at[Resource + D]
      _ <- pure[Resource + D, Unit](()).map(_ => throw Boom())
    yield a))
    assertEquals(log.toList, List("release a"))
  }

  test("on a machine: an abort through the scope releases it before it leaves (resource-abort-releases)") {
    val log = scala.collection.mutable.ArrayBuffer.empty[String]
    val p = Shift.prompt[String]
    val r = !.run(Shift.run[String, Pure](Shift.push[String, Pure](p)(Resource.run[String, D](for
      a <- Resource.acquire("a")(x => log += s"release $x").at[Resource + D]
      _ <- Shift.abort[String, Unit, Pure](p)("aborted").at[Resource + D]
    yield a))))
    assertEquals(r, "aborted")
    // the dropped piece is discontinued: its scope sees the throw, releases, and the abort answers
    assertEquals(log.toList, List("release a"))
  }
