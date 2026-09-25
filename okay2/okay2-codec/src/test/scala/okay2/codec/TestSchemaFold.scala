package okay2.codec

/** `Schema.fold` is a catamorphism that ties the knot by IDENTITY: a
 * named node is folded once however many edges reach it, a back edge
 * reaches the algebra as `ref`, and a lazy carrier forced twice folds
 * once (okay-codec's TestSchemaFold, part 3; its JsonSchema parts wait
 * for okay2's JsonSchema). */
class TestSchemaFold extends munit.FunSuite {

  type K[A] = Unit

  test("a named node is folded exactly once however many edges reach it, and the back edge is ref") {
    val folded = scala.collection.mutable.ListBuffer.empty[String]
    val refs = scala.collection.mutable.ListBuffer.empty[String]
    val counting = new Schema.Algebra[K] {
      def int = (); def long = (); def double = (); def bool = (); def string = (); def char = (); def bytes = ()
      def bigInt = ()
      def option[A](o: Schema.SOption[A], of: () => Unit) = of()
      def list[A](l: Schema.SList[A], of: () => Unit) = of()
      def vector[A](v: Schema.SVector[A], of: () => Unit) = of()
      def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[K, Any])]) = {
        folded += p.name; fields.foreach(_._2())
      }
      def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[K, A])]) = {
        folded += su.name; cases.foreach(_._2())
      }
      def iso[A, B](iso: Schema.SIso[A, B], under: () => Unit) = under()
      def ref[A](name: String) = { refs += name; () }
    }
    Schema.fold(implicitly[Schema[Expr]])(counting)
    assertEquals(folded.toList, List("Expr", "Num", "Add"))
    assertEquals(refs.toList, List("Expr", "Expr"))
  }

  type W[A] = A => Unit

  test("an edge forced twice folds once") {
    var products = 0
    val alg = new Schema.Algebra[W] {
      def int = _ => (); def long = _ => (); def double = _ => (); def bool = _ => ()
      def string = _ => (); def char = _ => (); def bytes = _ => (); def bigInt = _ => ()
      def option[A](o: Schema.SOption[A], of: () => W[A]) = _.foreach(of())
      def list[A](l: Schema.SList[A], of: () => W[A]) = _.foreach(of())
      def vector[A](v: Schema.SVector[A], of: () => W[A]) = _.foreach(of())
      def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[W, Any])]) = {
        products += 1
        // the kernel's own cast, restated: parts(a)(i) IS fields(i)'s X
        a => p.parts(a).zip(fields).foreach { case (v, (_, e)) => e()(v.asInstanceOf[e.X]) }
      }
      def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[W, A])]) =
        // the kernel's own cast, restated: caseOf(a) names the case a IS
        a => { val e = cases(su.caseOf(a))._2; e()(a.asInstanceOf[e.X]) }
      def iso[A, B](iso: Schema.SIso[A, B], under: () => W[B]) = a => under()(iso.from(a))
      def ref[A](name: String) = _ => ()
    }
    val walk = Schema.fold(implicitly[Schema[Tree]])(alg)
    assertEquals(products, 1)
    val t = Tree("a", Vector(Tree("b", Vector(Tree("c", Vector.empty))), Tree("d", Vector.empty)))
    walk(t); walk(t)
    assertEquals(products, 1)
  }

  test("a schema met twice without a cycle is folded once and is not a ref") {
    final case class Address(city: String, zip: Int = 0)
    final case class Pair(a: Address, b: Address, c: Option[Address])
    implicit val address: Schema[Address] = Schema.derived
    implicit val pair: Schema[Pair] = Schema.derived
    var refs = 0
    val names = scala.collection.mutable.ListBuffer.empty[String]
    val alg = new Schema.Algebra[K] {
      def int = (); def long = (); def double = (); def bool = (); def string = (); def char = (); def bytes = ()
      def bigInt = ()
      def option[A](o: Schema.SOption[A], of: () => Unit) = of()
      def list[A](l: Schema.SList[A], of: () => Unit) = of()
      def vector[A](v: Schema.SVector[A], of: () => Unit) = of()
      def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[K, Any])]) = {
        names += p.name; fields.foreach(_._2())
      }
      def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[K, A])]) = cases.foreach(_._2())
      def iso[A, B](iso: Schema.SIso[A, B], under: () => Unit) = under()
      def ref[A](name: String) = refs += 1
    }
    Schema.fold(pair)(alg)
    assertEquals(refs, 0)
    assertEquals(names.toList, List("Pair", "Address"))
  }
}
