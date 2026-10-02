package okay.cats

/**
 * JVM ONLY, and the reason is a measured bound, not a convenience
 * (sprint eager-carrier-depth): through an EAGER carrier the derived
 * `tailRecM` nests the host stack, which the JVM's Cont runner survives
 * by moving to a fresh stack; Scala.js has none and overflows between
 * 300 and 1 000 iterations.
 */
class TestToCatsDepth extends munit.FunSuite with okay.testkit.Munit.Diagnosed {
  import ToCats.given

  test("cats' Monad from okay's: tailRecM a million deep through an EAGER okay monad") {
    // an okay Monad cats has never heard of, eager like Option
    final case class Box[A](a: A)
    given okay.Monad[Box] with
      def pure[A](a: A): Box[A] = Box(a)
      extension [A](b: Box[A]) def flatMap[B](f: A => Box[B]): Box[B] = f(b.a)
    val C = summon[_root_.cats.Monad[Box]]
    assertEquals(C.tailRecM(0)(i => Box(if i < 1000000 then Left(i + 1) else Right(i))), Box(1000000))
    assertEquals(C.flatMap(Box(20))(x => Box(x + 1)), Box(21))
  }
}
