package okay2

/**
 * A row whose members can be told apart (spec stage 10). A split tests
 * one signature by its CLASS and takes the rest by exclusion, so two
 * signatures of one class — `Ask[Int] + Ask[String]` — are two types to
 * the row and one to the split: the second's operations go to the
 * first's handler. `Distinct[R]` refuses that row at compile time where
 * a union of handlers is built.
 */
class TestDistinct extends munit.FunSuite {
  import TestDistinct._

  test("the defect, as it was: a union over two Asks answers the String ask with the Int handler") {
    val both: Handler[Ask[Int] + Ask[String]] =
      Handler.union[Ask[Int], Ask[String]](Ask.effect[Int], intAsk, stringAsk, Distinct.unchecked)
    val p: String ! (Ask[Int] + Ask[String]) = Ask.ask[String]
    val e = intercept[ClassCastException](p.runWith(both).length)
    assert(e.getMessage.contains("Integer"), e.getMessage)
  }

  test("refused: a union of two signatures of one class does not compile") {
    val errors = compileErrors("okay2.Handler.union[okay2.TestDistinct.Ask[Int], okay2.TestDistinct.Ask[String]]")
    assert(errors.contains("TWO SIGNATURES OF ONE CLASS"), errors)
    assert(compileErrors("implicitly[okay2.Distinct[okay2.State[Int] + okay2.Writer[String] + okay2.State[String]]]").contains("TWO SIGNATURES OF ONE CLASS"))
  }

  test("admitted: distinct classes, a repeated member, the empty row, an abstract part") {
    val _ = implicitly[Distinct[State[Int] + Writer[String] + Reader[Int]]]
    val _ = implicitly[Distinct[State[Int] + State[Int]]]
    val _ = implicitly[Distinct[Pure]]
    def generic[F <: Row]: Distinct[State[Int] + F] = implicitly[Distinct[State[Int] + F]]
    val _ = generic[Writer[String]]
    // and a real union still builds
    val h: Handler[Ask[Int] + Produce] = Handler.union[Ask[Int], Produce]
    assertEquals((Ask.ask[Int]: Int ! (Ask[Int] + Produce)).runWith(h), 1)
  }
}

object TestDistinct {
  /** a parameterised signature tested by class, like State */
  sealed trait Ask[T] extends Row { type Op[+A] = Ask.Get[T] }
  object Ask {
    final case class Get[T]()
    implicit def effect[T]: Effect[Ask[T]] = Effect.of[Ask[T]]
    def ask[T]: T ! Ask[T] = Free.inject[Ask[T], T](Get[T]())
  }
  implicit val intAsk: Handler[Ask[Int]] = new Handler.Of[Ask[Int]] {
    def handle[A](a: Ask.Get[Int]): A = (1: Any).asInstanceOf[A]
  }
  implicit val stringAsk: Handler[Ask[String]] = new Handler.Of[Ask[String]] {
    def handle[A](a: Ask.Get[String]): A = ("s": Any).asInstanceOf[A]
  }
}
