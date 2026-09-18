package okay.ui

import okay.*
import okay.given

/**
 * `Ui`'s traversals are applicative-POLYMORPHIC, and this pins what
 * that buys (specs/applicative-do.md, specs/validated.md).
 *
 * `everywhere` and `shown` are declared `(F: Applicative[F]) ?=>`, so
 * a carrier written years later works at them with no change to this
 * module. `Validated` is that carrier: walking a tree with it reports
 * EVERY bad node instead of the first, which is what a form wants and
 * what `Either` cannot give.
 *
 * The property held by construction the day `Validated` landed and
 * nothing recorded it. Now something does.
 */
class TestUiApplicative extends munit.FunSuite {

  private given Semigroup[Seq[String]] with
    def combine(x: Seq[String], y: Seq[String]): Seq[String] = x ++ y

  private val tree = Ui.Column(Vector(
    Ui.Text("ok"), Ui.Text(""), Ui.Text("also ok"), Ui.Text("")))

  private def check(u: Ui): Validated[Seq[String], Ui] = u match
    case Ui.Text(s, _) if s.isEmpty => Validated.Invalid(Seq("empty text"))
    case other => Validated.Valid(other)

  test("a traversal at Validated reports EVERY bad node, not the first") {
    assertEquals(Ui.everywhere.traverseOf(check)(tree),
      Validated.Invalid(Seq("empty text", "empty text")))
  }

  test("the same walk at Either stops at the first — which is the comparison") {
    given Monad[[A] =>> Either[Seq[String], A]] with
      def pure[A](a: A): Either[Seq[String], A] = Right(a)
      extension [A](e: Either[Seq[String], A])
        def flatMap[B](f: A => Either[Seq[String], B]): Either[Seq[String], B] = e.flatMap(f)
    def checkE(u: Ui): Either[Seq[String], Ui] = check(u).toEither
    assertEquals(Ui.everywhere.traverseOf(checkE)(tree), Left(Seq("empty text")))
  }

  test("an all-valid walk rebuilds the tree unchanged") {
    val good = Ui.Column(Vector(Ui.Text("a"), Ui.Text("b")))
    assertEquals(Ui.everywhere.traverseOf(check)(good), Validated.Valid(good))
  }
}
