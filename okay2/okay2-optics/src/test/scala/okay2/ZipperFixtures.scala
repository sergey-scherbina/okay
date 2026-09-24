package okay2

import okay2.Optic._

/** the trees and records the zipper suites walk, at the top level */
object ZipperFixtures {
  final case class Rose(label: String, kids: Vector[Rose] = Vector.empty)

  implicit val hand: Plate[Rose] = new Plate[Rose] {
    def children(t: Rose): Vector[Rose] = t.kids
    def withChildren(t: Rose, cs: Vector[Rose]): Rose = t.copy(kids = cs)
  }

  /** the same tree as a self-traversal, for `Plate.of` */
  val kids: Traversal[Rose, Rose, Rose, Rose] = Traversal(new Walk[Rose, Rose, Rose, Rose] {
    def apply[F[_]](f: Rose => F[Rose])(implicit F: Applicative[F]): Rose => F[Rose] = r =>
      F.fmap(r.kids.foldLeft(F.pure(Vector.empty[Rose]))((acc, k) => F.app(F.fmap(acc, (v: Vector[Rose]) => (b: Rose) => v :+ b), f(k))),
        (ks: Vector[Rose]) => r.copy(kids = ks))
  })

  final case class Address(city: String, zip: Int)
  final case class Customer(name: String, address: Address)
  sealed trait Line
  object Line {
    final case class Item(sku: String, qty: Int) extends Line
    final case class Discount(pct: Int) extends Line
  }
  final case class Order(id: Long, customer: Customer, lines: Vector[Line])
  final case class Box[A](item: A, tag: String)
  final case class Pair(a: String, b: String)
}
