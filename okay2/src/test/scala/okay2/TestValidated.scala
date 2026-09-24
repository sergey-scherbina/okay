package okay2

import Validated._

/**
 * Every error, not the first. The deciding test is the PAIR: the same
 * `traverse`, the same leaves, at two carriers — at `Either` it reports
 * one problem, at `Validated` all of them. The Scala 3 core's suite.
 */
class TestValidated extends munit.FunSuite {

  /** an accumulation that is NOT a list and NOT commutative, so a test
   * cannot pass by accident on `++` */
  private implicit val semiString: Semigroup[String] = new Semigroup[String] {
    def combine(x: String, y: String): String = s"$x; $y"
  }

  private implicit val semiVector: Semigroup[Vector[String]] = new Semigroup[Vector[String]] {
    def combine(x: Vector[String], y: Vector[String]): Vector[String] = x ++ y
  }

  private def checkV(n: Int): Validated[Vector[String], Int] =
    if (n % 2 == 0) Valid(n) else Invalid(Vector(s"$n is odd"))

  private def checkE(n: Int): Either[Vector[String], Int] =
    if (n % 2 == 0) Right(n) else Left(Vector(s"$n is odd"))

  test("THE PAIR: one traverse, two carriers — all the errors, or the first") {
    val xs = Seq(1, 2, 3, 4, 5)
    val collected = traverse(xs)(checkV)
    assertEquals(collected, Invalid(Vector("1 is odd", "3 is odd", "5 is odd")))

    implicit val eitherMonad: Monad[({ type L[A] = Either[Vector[String], A] })#L] =
      new Monad[({ type L[A] = Either[Vector[String], A] })#L] {
        def pure[A](a: A): Either[Vector[String], A] = Right(a)
        def flatMap[A, B](e: Either[Vector[String], A])(f: A => Either[Vector[String], B]): Either[Vector[String], B] = e.flatMap(f)
      }
    assertEquals(traverse(xs)(checkE), Left(Vector("1 is odd")))
  }

  test("all valid: the answers and their order are the Either ones") {
    val xs = Seq(2, 4, 6)
    assertEquals(traverse(xs)(checkV), Valid(Seq(2, 4, 6)))
    assertEquals(traverse(xs)(checkV).toEither, Right(Seq(2, 4, 6)))
  }

  test("the applicative laws, on a NON-COMMUTATIVE accumulation") {
    val A = implicitly[Applicative[({ type L[X] = Validated[String, X] })#L]]
    import A.pure
    val v: Validated[String, Int] = Valid(21)
    val u: Validated[String, Int => Int] = Valid((_: Int) * 2)
    val w: Validated[String, Int => Int] = Valid((_: Int) + 1)

    assertEquals(A.app(pure(identity[Int] _), v), v)
    assertEquals(A.app(pure((x: Int) => x + 1), pure(41)), pure(42))
    assertEquals(A.app(u, pure(21)), A.app(pure((f: Int => Int) => f(21)), u))
    val compose = (f: Int => Int) => (g: Int => Int) => f.compose(g)
    assertEquals(A.app(A.app(A.app(pure(compose), u), w), v), A.app(u, A.app(w, v)))

    val e1: Validated[String, Int => Int] = Invalid("first")
    val e2: Validated[String, Int] = Invalid("second")
    assertEquals(A.app(e1, e2), Invalid("first; second"))
  }

  test("andThen short-circuits, and that is why there is no Monad") {
    var ran = 0
    def next(n: Int): Validated[Vector[String], Int] = { ran += 1; Valid(n + 1) }

    assertEquals(Invalid(Vector("no")).andThen(next), Invalid(Vector("no")))
    assertEquals(ran, 0)
    assertEquals(Valid(41).andThen(next), Valid(42))
    assertEquals(ran, 1)

    assert(compileErrors("implicitly[okay2.Monad[({ type L[A] = okay2.Validated[Vector[String], A] })#L]]").nonEmpty,
      "a Monad[Validated] exists — every traverse now stops at the first error")
  }

  test("the accumulation is the caller's: a COUNT, not a list") {
    implicit val count: Semigroup[Int] = new Semigroup[Int] { def combine(x: Int, y: Int): Int = x + y }
    def one(n: Int): Validated[Int, Int] = if (n % 2 == 0) Valid(n) else Invalid(1)
    assertEquals(traverse(Seq(1, 2, 3, 4, 5))(one), Invalid(3))
  }

  test("the doors between the roads") {
    assertEquals(Validated.fromEither(Right(1)), Valid(1))
    assertEquals(Validated.fromEither(Left("no")), Invalid("no"))
    assertEquals(Valid(1).getOrElse(0), 1)
    assertEquals(Invalid("no").getOrElse(0), 0)
    assertEquals(Valid(1).isValid, true)
  }
}
