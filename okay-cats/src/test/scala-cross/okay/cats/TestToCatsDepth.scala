package okay.cats

/**
 * cats' `Monad` from an okay monad, deep, on every platform
 * (specs/eager-carrier-depth.md). The carrier is EAGER, so it brings its
 * own loop — `TailRecM` is the carrier's, never derived — and with it
 * the bridge's `tailRecM` holds a million on Scala.js too, where the
 * derivation this replaced overflowed at 300-1 000.
 */
class TestToCatsDepth extends munit.FunSuite with okay.testkit.Munit.Diagnosed {
  import ToCats.given

  final case class Box[A](a: A)
  given okay.Monad[Box] with
    def pure[A](a: A): Box[A] = Box(a)
    extension [A](b: Box[A]) def flatMap[B](f: A => Box[B]): Box[B] = f(b.a)
  given okay.TailRecM[Box] with
    def tailRecM[A, B](a: A)(f: A => Box[Either[A, B]]): Box[B] =
      @scala.annotation.tailrec def loop(s: A): Box[B] = f(s).a match
        case Left(next) => loop(next)
        case Right(b) => Box(b)
      loop(a)

  test("cats' Monad from okay's: tailRecM a million deep through an EAGER okay monad") {
    val C = summon[_root_.cats.Monad[Box]]
    assertEquals(C.tailRecM(0)(i => Box(if i < 1000000 then Left(i + 1) else Right(i))), Box(1000000))
    assertEquals(C.flatMap(Box(20))(x => Box(x + 1)), Box(21))
  }
}
