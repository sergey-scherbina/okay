package okay

import !.*

import scala.util.chaining.*

class TestGenerate extends munit.FunSuite {

  // the million-element run proves STACK safety, not speed; under a
  // full-family parallel run a 30s default has twice been the only
  // thing that failed, so the clock gets room the property does not need
  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  test("fibs by laziness: LazyList") {
    println(fibs[BigInt, LazyList].take(1000).force)
    assertEquals(fibs[Int, LazyList].take(10).toList,
      List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  test("fibs by effects: Producer, pure and logged") {
    val p = fibs[BigInt, Producer]
    val n = 100
    val x = p.next(n).peek.tap(println)
    val y = p.next(n)(using Producer.log()).peek.tap(println)
    assertEquals(x, y)
  }

  test("stack safety: 1M produced values") {
    fibs[BigInt, Producer].next(1000000).peek.tap(println)
  }

  test("fibs by effects: Feed, the fourth carrier") {
    def firstN[W](s: Feed[W], n: Int): Vector[W] =
      if n <= 0 then Vector.empty
      else Writer.uncons(s) match
        case Left(_) => Vector.empty
        case Right((w, rest)) => w +: firstN(rest, n - 1)
    assertEquals(firstN(nats[Int, Feed], 5), Vector(0, 1, 2, 3, 4))
    assertEquals(firstN(fibs[Long, Feed], 10),
      Vector(0L, 1L, 1L, 2L, 3L, 5L, 8L, 13L, 21L, 34L))
  }

  test("pure(a) at Feed compiles no differently than at Producer — the difference is a WARNING, verified by hand") {
    // Produce: the identity signature makes the element type the
    // answer type, so `pure(a)` is an ORDINARY well-typed answer and
    // produces nothing — nothing distinguishes it from a Producer
    // that honestly ends with `a`
    val silent: Producer[Int] = pure(5)
    var seen = 0
    Stream.fold(silent)(using Fold[Int, Unit](())((_, _) => seen += 1))
    assertEquals(seen, 0)
    assert(compileErrors("val p: okay.Producer[Int] = okay.pure(5)").isEmpty)

    // Feed: the answer is always Unit, so `pure(5)` needs Scala's own
    // value-discard adaptation to compile at all — this ALSO produces
    // no hard error (`compileErrors` cannot tell the two apart: it
    // reports errors only, and munit's own macro drops warnings
    // entirely, checked directly against the classic `val u: Unit = 5`
    // shape before trusting this). What actually differs is a real
    // compile's diagnostics, not this assertion:
    //   sbt okayStreamJVM/compile on `val f: Feed[Int] = pure(5)` prints
    //   [E190] Potential Issue Warning: Discarded non-Unit value of
    //   type Int. Add `: Unit` to discard silently.
    // which this repo's gate refuses as any other warning. So the
    // trap is not closed by a TYPE the compiler enforces here — it is
    // made INSPECTABLE at the point Producer's identical-looking
    // `pure(a)` gives the reviewer nothing to see at all.
    assert(compileErrors("val f: okay.Feed[Int] = okay.pure(5)").isEmpty)

    // the honest way to end an empty Feed still compiles clean, no
    // adaptation needed since the value already IS Unit
    val empty: Feed[Int] = pure(())
    assertEquals(Writer.uncons(empty), Left(()))
  }

  test("Feed's specialized iterator agrees with Writer.collect on every tree shape") {
    val St = feedStream[Unit]
    def walk[W](f: Feed[W]): Vector[W] = St.iterator(f).toVector
    def oracle[W](f: Feed[W]): Vector[W] = Writer.collect[W, Unit, okay.Pure](f).runWith._1

    // empty: Pure only
    assertEquals(walk(pure(()): Feed[Int]), Vector.empty)
    // a bare tell: the terminal Inject(Say) with no continuation
    assertEquals(walk(Writer.tell(7)), Vector(7))
    // right-nested binds, the shape generate/tell.flatMap build
    val right: Feed[Int] = Writer.tell(1).flatMap(_ => Writer.tell(2)).flatMap(_ => Writer.tell(3))
    // left-nested binds and a leading pure: the `case _ => resume` arm
    val left: Feed[Int] = pure(()).flatMap(_ => (Writer.tell(1).flatMap(_ => Writer.tell(2))).flatMap(_ => Writer.tell(3)))
    // a tell followed by a pure that is NOT the end, then more tells
    val mid: Feed[Int] = Writer.tell(1).flatMap(_ => pure(())).flatMap(_ => Writer.tell(2).map(_ => ()))
    for f <- List(right, left, mid) do
      assertEquals(walk(f), oracle(f))
    assertEquals(walk(right), Vector(1, 2, 3))
    assertEquals(walk(mid), Vector(1, 2))
    // laziness: an infinite feed yields on demand
    assertEquals(St.iterator(nats[Long, Feed]).take(5).toVector, Vector(0L, 1L, 2L, 3L, 4L))
    // and the long walk is stack-safe
    assertEquals(St.iterator(nats[Int, Feed]).drop(1000000).next(), 1000000)
  }

}
