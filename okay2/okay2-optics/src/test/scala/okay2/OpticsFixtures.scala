package okay2

/** the records the optics suites use: at the top level, since a case
 * class nested in a suite trips -Xlint's outer-reference check */
object OpticsFixtures {
  final case class Address(city: String, zip: Int)
  final case class Person(name: String, age: Int, address: Option[Address])

  sealed trait Shape
  final case class Circle(r: Double) extends Shape
  final case class Square(side: Double) extends Shape

  final case class Counter(n: Int, label: String)
  final case class App(counter: Counter, user: String)

  final case class Wrapped(v: Int)
  final case class Box[A](item: A, tag: String)

  final case class Point(label: String, xs: LazyList[Double])

  final case class Line(sku: String, qty: Int)
  final case class Order(id: String, lines: Vector[Line])

  /** a price lookup, the leaf a Static walk lists */
  sealed trait Prices extends Row { type Op[+A] = Prices.Op[A] }
  object Prices {
    sealed trait Op[+A]
    final case class Of(sku: String) extends Op[Int]
    implicit val effect: Effect[Prices] = Effect.of[Prices]
  }

  final case class Batch[A](skus: Vector[String], run: Map[String, Int] => A)
  implicit val batchSelective: Selective[Batch] = new Selective[Batch] {
    def pure[A](a: A): Batch[A] = Batch(Vector.empty, _ => a)
    def app[A, B](f: Batch[A => B], a: Batch[A]): Batch[B] = Batch(f.skus ++ a.skus, m => f.run(m)(a.run(m)))
    def select[A, B](e: Batch[Either[A, B]], f: => Batch[A => B]): Batch[B] = {
      val g = f
      Batch(e.skus ++ g.skus, m => e.run(m).fold(g.run(m), identity))
    }
  }

  /** scalac 2 does not refine a method's type parameter by a constructor
   * pattern, so a handler's answer is asserted — TestStatic's claim */
  def answer[A](x: Any): A = x.asInstanceOf[A]
}
