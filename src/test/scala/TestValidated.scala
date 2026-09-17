package okay

/**
 * Every error, not the first (specs/validated.md).
 *
 * The deciding test is the PAIR: the same `traverse`, the same
 * leaves, at two carriers. At `Either` it reports one problem; at
 * `Validated` it reports all of them. Nothing in the program moves.
 */
class TestValidated extends munit.FunSuite {

  import Validated.*

  /** an accumulation that is NOT a list and NOT commutative, so a
   * test cannot pass by accident on `++` */
  private given Semigroup[String] with
    def combine(x: String, y: String): String = s"$x; $y"

  private given Semigroup[Vector[String]] with
    def combine(x: Vector[String], y: Vector[String]): Vector[String] = x ++ y

  private def checkV(n: Int): Validated[Vector[String], Int] =
    if n % 2 == 0 then Valid(n) else Invalid(Vector(s"$n is odd"))

  private def checkE(n: Int): Either[Vector[String], Int] =
    if n % 2 == 0 then Right(n) else Left(Vector(s"$n is odd"))

  test("THE PAIR: one traverse, two carriers — all the errors, or the first") {
    val xs = Seq(1, 2, 3, 4, 5)

    val collected = traverse(xs)(checkV)
    assertEquals(collected, Invalid(Vector("1 is odd", "3 is odd", "5 is odd")))

    // the same program at Either stops at the first
    given Monad[[A] =>> Either[Vector[String], A]] with
      def pure[A](a: A): Either[Vector[String], A] = Right(a)
      extension [A](e: Either[Vector[String], A])
        def flatMap[B](f: A => Either[Vector[String], B]): Either[Vector[String], B] = e.flatMap(f)
    assertEquals(traverse(xs)(checkE), Left(Vector("1 is odd")))
  }

  test("all valid: the answers and their order are the Either ones") {
    val xs = Seq(2, 4, 6)
    assertEquals(traverse(xs)(checkV), Valid(Seq(2, 4, 6)))
    assertEquals(traverse(xs)(checkV).toEither, Right(Seq(2, 4, 6)))
  }

  test("the applicative laws, on a NON-COMMUTATIVE accumulation") {
    val A = summon[Applicative[[X] =>> Validated[String, X]]]
    import A.pure
    val v: Validated[String, Int] = Valid(21)
    val u: Validated[String, Int => Int] = Valid((_: Int) * 2)
    val w: Validated[String, Int => Int] = Valid((_: Int) + 1)

    assertEquals(pure(identity[Int]).app(v), v)
    assertEquals(pure((x: Int) => x + 1).app(pure(41)), pure(42))
    assertEquals(u.app(pure(21)), pure((f: Int => Int) => f(21)).app(u))
    val compose = (f: Int => Int) => (g: Int => Int) => f.compose(g)
    assertEquals(pure(compose).app(u).app(w).app(v), u.app(w.app(v)))

    // and the accumulation itself keeps program order, which a
    // commutative combine could not have told us
    val e1: Validated[String, Int => Int] = Invalid("first")
    val e2: Validated[String, Int] = Invalid("second")
    assertEquals(e1.app(e2), Invalid("first; second"))
  }

  test("andThen short-circuits, and that is why there is no Monad") {
    var ran = 0
    def next(n: Int): Validated[Vector[String], Int] = { ran += 1; Valid(n + 1) }

    assertEquals(Invalid(Vector("no")).andThen(next), Invalid(Vector("no")))
    assertEquals(ran, 0)
    assertEquals(Valid(41).andThen(next), Valid(42))
    assertEquals(ran, 1)

    // the type refuses the instance that would undo the collecting
    assert(compileErrors("summon[Monad[[A] =>> Validated[Vector[String], A]]]").nonEmpty,
      "a Monad[Validated] exists — every traverse now stops at the first error")
  }

  test("the accumulation is the caller's: a COUNT, not a list") {
    given Semigroup[Int] with
      def combine(x: Int, y: Int): Int = x + y
    def count(n: Int): Validated[Int, Int] = if n % 2 == 0 then Valid(n) else Invalid(1)
    assertEquals(traverse(Seq(1, 2, 3, 4, 5))(count), Invalid(3))
  }

  test("the doors between the roads") {
    assertEquals(Validated.fromEither(Right(1)), Valid(1))
    assertEquals(Validated.fromEither(Left("no")), Invalid("no"))
    assertEquals(Valid(1).getOrElse(0), 1)
    assertEquals(Invalid("no").getOrElse(0), 0)
    assertEquals(Valid(1).isValid, true)
  }
}
