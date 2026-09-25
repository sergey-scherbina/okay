package okay2.codec

// Shared test models. A sealed type's children are declared before
// any derivation reads them (SI-7046), and a RECURSIVE type declares
// its schema by name, in its companion, where the derivation's own
// field lookup finds it.

sealed trait Colour
object Colour {
  case object Red extends Colour
  case object Green extends Colour
}

sealed trait Expr
object Expr {
  final case class Num(n: Int) extends Expr
  final case class Add(l: Expr, r: Expr) extends Expr
  implicit lazy val schema: Schema[Expr] = Schema.derived
}

final case class Tree(label: String, kids: Vector[Tree])
object Tree {
  implicit lazy val schema: Schema[Tree] = Schema.derived
}

final case class Kids(kids: Vector[Kids])
object Kids {
  implicit lazy val schema: Schema[Kids] = Schema.derived
}

sealed trait Chain
object Chain {
  case object Leaf extends Chain
  final case class Node(next: Chain) extends Chain
  implicit lazy val schema: Schema[Chain] = Schema.derived
}

sealed trait Pet
object Pet {
  final case class Dog(name: String, age: Int) extends Pet
  final case class Cat(name: String) extends Pet
}
