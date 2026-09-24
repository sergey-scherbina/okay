package okay2

import okay2.Optic._
import OpticsFixtures._

/** ONE OPTIC, SEVERAL EFFECTS — and no code in the optics for any of
 * them: `traverseOf` asks for an Applicative and nothing more, so
 * `Validated` reports every bad focus and `Static` lists what the walk
 * would do before it does it. The Scala 3 core's TestOpticCarriers
 * (its `Par` case needs okay2-async, which this module does not
 * depend on). */
class TestOpticCarriers extends munit.FunSuite {

  implicit val lines: Semigroup[Seq[String]] = new Semigroup[Seq[String]] {
    def combine(x: Seq[String], y: Seq[String]): Seq[String] = x ++ y
  }

  type V[X] = Validated[Seq[String], X]
  type E[X] = Either[Seq[String], X]
  type St[X] = Static[Prices, X]

  private val eachLine: Traversal[Order, Order, Line, Line] = Lens[Order](_.lines).andThen(Traversal.each[Line, Line])
  private val order = Order("A-1", Vector(Line("pen", 2), Line("ink", 0), Line("pad", -3)))

  def check(l: Line): V[Line] =
    if (l.qty > 0) Validated.Valid(l) else Validated.Invalid(Seq(s"${l.sku}: qty must be > 0, got ${l.qty}"))

  test("Validated: the walk reports EVERY bad line, not the first") {
    assertEquals(eachLine.traverseOf[V](check)(order),
      Validated.Invalid(Seq("ink: qty must be > 0, got 0", "pad: qty must be > 0, got -3")): V[Order])
    // the same walk at Either stops at the first
    implicit val either: Monad[E] = new Monad[E] {
      def pure[A](a: A): E[A] = Right(a)
      def flatMap[A, B](e: E[A])(f: A => E[B]): E[B] = e.flatMap(f)
    }
    assertEquals(eachLine.traverseOf[E](l => check(l).toEither)(order), Left(Seq("ink: qty must be > 0, got 0")): E[Order])
  }

  test("Validated: a good order comes back rebuilt, not merely accepted") {
    assertEquals(eachLine.traverseOf[V](l => Validated.Valid(l.copy(qty = l.qty * 10)))(Order("B", Vector(Line("pen", 1)))),
      Validated.Valid(Order("B", Vector(Line("pen", 10)))): V[Order])
  }

  private def priceLine(l: Line): St[Line] = Static.op[Prices, Int](Prices.Of(l.sku)).map(p => l.copy(qty = p))

  test("Static: the operations a walk would perform, listed before running") {
    val spine = eachLine.traverseOf[St](priceLine)(order)
    assertEquals(spine.leaves, Vector[Any](Prices.Of("pen"), Prices.Of("ink"), Prices.Of("pad")))
    implicit val h: Handler[Prices] = new Handler[Prices] {
      def handle[A](e: Prices.Op[A]): A = e match { case Prices.Of(sku) => answer[A](sku.length) }
    }
    assertEquals(Effects.runFree(spine.toFree), Order("A-1", Vector(Line("pen", 3), Line("ink", 3), Line("pad", 3))))
  }

  test("Static: the plan is a VALUE, so it can be batched — one round trip for every focus") {
    val spine = eachLine.traverseOf[St](priceLine)(order)
    val toBatch = new Static.To[Prices, Batch] {
      def apply[X](op: Prices.Op[X]): Batch[X] = op match { case Prices.Of(sku) => Batch(Vector(sku), m => answer[X](m.getOrElse(sku, 0))) }
    }
    var calls = 0
    val plan = spine.foldMap(toBatch)
    assertEquals(plan.skus, Vector("pen", "ink", "pad"))
    val answers = plan.run { calls += 1; plan.skus.map(s => s -> s.length).toMap }
    assertEquals(calls, 1)
    assertEquals(answers, Order("A-1", Vector(Line("pen", 3), Line("ink", 3), Line("pad", 3))))
  }
}
