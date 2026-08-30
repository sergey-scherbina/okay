package okay.cats

import okay.{!, %, +, Async, Produce, Throws, async, effect, produce, pure}
import okay.!.*
import okay.given
import CatsInterop.*
import _root_.cats.syntax.all.*
import _root_.cats.effect.unsafe.implicits.global

class TestCatsInterop extends munit.FunSuite {

  test("okay programs are cats monads: syntax and traverse work") {
    val p: Int ! Produce = (produce(20), produce(22)).mapN(_ + _)
    assertEquals(p.runWith, 42)
    val t: List[Int] ! Produce = List(1, 2, 3).traverse(x => produce(x * 10))
    assertEquals(t.runWith, List(10, 20, 30))
  }

  test("tailRecM is stack-safe") {
    val M = summon[_root_.cats.Monad[[A] =>> A ! Produce]]
    val r = M.tailRecM(0)(i =>
      produce(if i < 100000 then Left(i + 1) else Right(i)))
    assertEquals(r.runWith, 100000)
  }

  test("MonadError over Throws: raise and recover") {
    type P[A] = A ! (Throws % String + Produce)
    val ME = summon[_root_.cats.MonadError[P, String]]
    val bad: P[Int] = ME.raiseError("boom")
    assertEquals(okay.runEither[Int, Produce, String](
      ME.handleErrorWith(bad)(e => pure(e.length))).runWith, Right(4))
    assertEquals(okay.runEither[Int, Produce, String](
      ME.handleErrorWith(pure(7): P[Int])(_ => pure(0))).runWith, Right(7))
  }

  test("Async and IO bridge both ways") {
    assertEquals(toIO(async(40).map(_ + 2)).unsafeRunSync(), 42)
    assertEquals(fromIO(_root_.cats.effect.IO(21).map(_ * 2)).runWith, 42)
  }

  test("our Scheduler runs on the cats-effect runtime") {
    given okay.Scheduler = CatsInterop.scheduler
    val f = okay.Async.spawn(okay.async { Thread.sleep(10); 21 })
    assertEquals(f.join() * 2, 42)
    assertEquals(okay.Async.par(okay.async(1), okay.async(2)).runWith, (1, 2))
  }

  test("Free converts to cats free and back, operation for operation") {
    val p: Int ! Produce = produce(1).flatMap(x => produce(x + 1)).map(_ * 10)
    val c = toCats(p)
    assertEquals(fromCats(c).runWith, 20)
    // steps preserved: the cats side folds the same two operations
    var seen = List.empty[Any]
    val counted = c.foldMap(new _root_.cats.arrow.FunctionK[Produce, _root_.cats.Id]:
      def apply[X](fx: Produce[X]): X = { seen ::= fx; fx })
    assertEquals(counted, 20)
    assertEquals(seen.length, 2)
  }
}
