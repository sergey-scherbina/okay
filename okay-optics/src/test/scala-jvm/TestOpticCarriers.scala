package okay

import okay.given
import java.util.concurrent.{CountDownLatch, TimeUnit}

/**
 * ONE OPTIC, THREE EFFECTS — and no code in the optics for any of
 * them.
 *
 * A traversal's signature is `traverseOf[F](f: A => F[B]): S => F[T]`
 * with `Applicative[F]` and nothing more, so the applicative slot is
 * where the effect goes. Every carrier of the applicative arc drops
 * into it: `Validated` reports every bad focus instead of the first,
 * `Par` visits the foci at once, and `Static` says what the walk WOULD
 * do before it does it.
 *
 * These properties held by construction from the day each carrier
 * landed. This file is what records them, and it doubles as the
 * worked example the guide points at.
 */
class TestOpticCarriers extends munit.FunSuite {

  private given Semigroup[Seq[String]] with
    def combine(x: Seq[String], y: Seq[String]): Seq[String] = x ++ y

  final case class Line(sku: String, qty: Int)
  final case class Order(id: String, lines: Vector[Line])

  /** the optic under test: every line of an order */
  private val eachLine: Traversal[Order, Order, Line, Line] =
    Lens[Order](_.lines).andThen(Traversal.each[Line, Line])

  private val order = Order("A-1", Vector(Line("pen", 2), Line("ink", 0), Line("pad", -3)))

  // ------------------------------------------------------------
  // 1. Validated: every problem, with the line that caused it

  test("Validated: the walk reports EVERY bad line, not the first") {
    def check(l: Line): Validated[Seq[String], Line] =
      if l.qty > 0 then Validated.Valid(l)
      else Validated.Invalid(Seq(s"${l.sku}: qty must be > 0, got ${l.qty}"))

    assertEquals(
      eachLine.traverseOf[[X] =>> Validated[Seq[String], X]](check)(order),
      Validated.Invalid(Seq("ink: qty must be > 0, got 0", "pad: qty must be > 0, got -3")))

    // the same walk at Either stops at the first — the comparison that
    // makes the line above mean something
    given Monad[[A] =>> Either[Seq[String], A]] with
      def pure[A](a: A): Either[Seq[String], A] = Right(a)
      extension [A](e: Either[Seq[String], A])
        def flatMap[B](f: A => Either[Seq[String], B]): Either[Seq[String], B] = e.flatMap(f)
    assertEquals(
      eachLine.traverseOf[[X] =>> Either[Seq[String], X]](l => check(l).toEither)(order),
      Left(Seq("ink: qty must be > 0, got 0")))
  }

  test("Validated: a good order comes back rebuilt, not merely accepted") {
    def check(l: Line): Validated[Seq[String], Line] = Validated.Valid(l.copy(qty = l.qty * 10))
    assertEquals(
      eachLine.traverseOf[[X] =>> Validated[Seq[String], X]](check)(Order("B", Vector(Line("pen", 1)))),
      Validated.Valid(Order("B", Vector(Line("pen", 10)))))
  }

  // ------------------------------------------------------------
  // 2. Par: the foci visited at once

  test("Par: every line is priced concurrently, and the order is rebuilt") {
    // the proof is a rendezvous, not a clock: each focus must meet the
    // others before it may finish, which a sequential walk cannot do
    val latch = CountDownLatch(3)
    val priced = eachLine.traverseOf[Par](l => Par(async {
      latch.countDown()
      val met = latch.await(10000, TimeUnit.MILLISECONDS)
      l.copy(qty = if met then l.qty + 100 else -1)
    }))(order)
    assertEquals(priced.seq.runWith,
      Order("A-1", Vector(Line("pen", 102), Line("ink", 100), Line("pad", 97))))
  }

  // ------------------------------------------------------------
  // 3. Static: what the walk WOULD do, before it does it

  enum Prices[+A]:
    case Of(sku: String) extends Prices[Int]

  private def priceLine(l: Line): Static[Prices, Line] =
    Static.op(Prices.Of(l.sku)).map(p => l.copy(qty = p))

  test("Static: the operations a walk would perform, listed before running") {
    val spine = eachLine.traverseOf[[X] =>> Static[Prices, X]](priceLine)(order)

    // read it WITHOUT running: a dry run, an audit, a batch plan
    assertEquals(spine.leaves, Vector(Prices.Of("pen"), Prices.Of("ink"), Prices.Of("pad")))

    // and then run the same value the ordinary way
    given Handler[Prices] with
      def handle[A](e: Prices[A]): A = e match
        case Prices.Of(sku) => sku.length
    assertEquals(spine.toFree.runWith,
      Order("A-1", Vector(Line("pen", 3), Line("ink", 3), Line("pad", 3))))
  }

  test("Static: the plan is a VALUE, so it can be batched") {
    // one round trip for every focus, because the operations are known
    final case class Batch[A](skus: Vector[String], run: Map[String, Int] => A)
    given Selective[Batch] with
      def pure[A](a: A): Batch[A] = Batch(Vector.empty, _ => a)
      extension [A, B](f: Batch[A => B])
        def app(a: Batch[A]): Batch[B] = Batch(f.skus ++ a.skus, m => f.run(m)(a.run(m)))
      extension [A, B](e: Batch[Either[A, B]])
        def select(f: => Batch[A => B]): Batch[B] =
          Batch(e.skus ++ f.skus, m => e.run(m).fold(f.run(m), identity))

    val spine = eachLine.traverseOf[[X] =>> Static[Prices, X]](priceLine)(order)

    val toBatch: Prices ==> Batch = [X] => (op: Prices[X]) => op match
      case Prices.Of(sku) => Batch(Vector(sku), m => m.getOrElse(sku, 0))

    var calls = 0
    val plan = spine.foldMap(toBatch)
    assertEquals(plan.skus, Vector("pen", "ink", "pad"))
    val answers = plan.run:
      calls += 1
      plan.skus.map(s => s -> s.length).toMap
    assertEquals(calls, 1)
    assertEquals(answers, Order("A-1", Vector(Line("pen", 3), Line("ink", 3), Line("pad", 3))))
  }
}
