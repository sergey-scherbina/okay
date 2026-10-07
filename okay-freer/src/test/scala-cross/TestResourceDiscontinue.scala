package okay.freer

import okay.*
import okay.given

import okay.freer.Row.*

/**
 * A DROPPED CONTINUATION RELEASES ITS SCOPES (resource-abort-releases, specs/handle-frames.md): an `abort` through a
 * `Resource` scope discontinues the piece it drops — `Shift.Discontinued` thrown into it, which the scope releases
 * on and a `try` declines — and a body that drops `k` itself says so with `Shift.discontinue(k)`.
 */
class TestResourceDiscontinue extends munit.FunSuite:

  type D = Shift % ? + Pure
  given Failing[D] = new Failing[D]:
    def guard[X](e: D[X], onFailure: () => Unit): D[X] = e

  final class Boom extends RuntimeException("boom")

  def scope[A](log: scala.collection.mutable.ArrayBuffer[String], name: String)(body: String => A ! Resource + D): A ! D =
    Resource.run[A, D](Resource.acquire(name)(x => log += s"release $x").at[Resource + D].flatMap(body))

  def tryD[A](fa: => A ! D)(h: Throwable => A ! D): A ! D = summon[CanTry[[X] =>> X ! D]].tryIn(fa)(h)

  test("an abort through two scopes releases both, inner first, and answers the value") {
    val log = scala.collection.mutable.ArrayBuffer.empty[String]
    val p = Shift.prompt[String]
    val r = !.run(Shift.run[String, Pure](Shift.push[String, Pure](p)(
      scope(log, "outer")(_ => scope(log, "inner")(_ =>
        Shift.abort[String, String, Pure](p)("aborted").at[Resource + D]).at[Resource + D]))))
    assertEquals(r, "aborted")
    assertEquals(log.toList, List("release inner", "release outer"))
  }

  test("an abort through a try and a scope: the try does not answer it, the scope releases") {
    val log = scala.collection.mutable.ArrayBuffer.empty[String]
    val p = Shift.prompt[String]
    val r = !.run(Shift.run[String, Pure](Shift.push[String, Pure](p)(
      scope(log, "a")(_ => tryD(Shift.abort[String, String, Pure](p)("aborted"))(t => {
        log += s"caught $t"; pure("recovered") }).at[Resource + D]))))
    assertEquals(r, "aborted")
    assertEquals(log.toList, List("release a"))
  }

  test("a release that fails during an abort: the abort fails with it") {
    val p = Shift.prompt[String]
    val e = intercept[Boom](!.run(Shift.run[String, Pure](Shift.push[String, Pure](p)(
      Resource.run[String, D](Resource.acquire("a")(_ => throw Boom()).at[Resource + D].flatMap(_ =>
        Shift.abort[String, String, Pure](p)("aborted").at[Resource + D]))))))
    assertEquals(e.getMessage, "boom")
  }

  test("an abort through no scope: the value, nothing run on the way") {
    val log = scala.collection.mutable.ArrayBuffer.empty[String]
    val p = Shift.prompt[String]
    val r = !.run(Shift.run[String, Pure](Shift.push[String, Pure](p)(
      tryD(Shift.abort[String, String, Pure](p)("aborted"))(t => { log += s"caught $t"; pure("recovered") }))))
    assertEquals(r, "aborted")
    assertEquals(log.toList, Nil)
  }

  test("a body that drops k says so: Shift.discontinue(k) releases the scope, the body's answer stands") {
    val log = scala.collection.mutable.ArrayBuffer.empty[String]
    val p = Shift.prompt[String]
    val r = !.run(Shift.run[String, Pure](Shift.push[String, Pure](p)(
      scope(log, "a")(_ => Shift.shift[String, Int, Pure](p)(k =>
        Shift.discontinue(k).map(_ => "dropped")).at[Resource + D].map(_.toString)))))
    assertEquals(r, "dropped")
    assertEquals(log.toList, List("release a"))
  }

  test("a stored k is not released at the capture: resumed later, it runs and releases at its end") {
    val log = scala.collection.mutable.ArrayBuffer.empty[String]
    val p = Shift.prompt[String]
    var stored: (Int => String ! D) | Null = null
    val r = !.run(Shift.run[String, Pure](Shift.push[String, Pure](p)(
      scope(log, "a")(a => Shift.shift[String, Int, Pure](p)(k => { stored = k; pure("paused") })
        .at[Resource + D].map(n => s"$a$n")))))
    assertEquals(r, "paused")
    assertEquals(log.toList, Nil)
    assertEquals(!.run(Shift.run[String, Pure](stored.nn(7))), "a7")
    assertEquals(log.toList, List("release a"))
  }

  test("a try whose handler throws anew, nothing under it: the new throw goes out, not the one it replaced") {
    final class Other extends RuntimeException("other")
    val e = intercept[Other](!.run(Shift.run[String, Pure](
      tryD(pure[D, Unit](()).map[String](_ => throw Boom()))(_ => throw Other()))))
    assertEquals(e.getMessage, "other")
  }
