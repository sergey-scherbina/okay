package okay

// the per-file import the direct colouring needs, for the reason
// `direct-given-import-needed` records and every other TestDirect*
// file in this directory carries
import okay.Direct.*
import scala.language.implicitConversions
import okay.given

/**
 * DO OPTICS AND ARROWS WORK INSIDE A `direct` BLOCK? (the operator's
 * question, 2026-09-18.) Asked as tests rather than answered from
 * memory, because the one boundary already recorded — from
 * `ui-direct-example` — is narrow and easy to over-read: "the focus
 * function cannot be written in the enclosing block: a mark under a
 * lambda is the corner direct's v1 refuses by design. So an optic and
 * a direct block meet at the CALL, not inside it."
 *
 * That sentence is about a `.reflect` INSIDE the focus function. It
 * says nothing about the ordinary cases below, and this file is what
 * tells them apart.
 */
class TestDirectOpticsArrows extends munit.FunSuite {

  final case class Address(city: String, zip: Int)
  final case class Person(name: String, age: Int, home: Address)

  private val age = Lens[Person](_.age)
  private val city = Lens[Person](_.home).andThen(Lens[Address](_.city))
  private val each = Traversal.each[Int, Int]

  private type V[A] = Validated[Vector[String], A]
  private given Semigroup[Vector[String]] with
    def combine(x: Vector[String], y: Vector[String]): Vector[String] = x ++ y

  private def check(p: Person): V[Person] =
    if p.age > 0 then Validated.Valid(p) else Validated.Invalid(Vector(s"${p.name}: age ${p.age}"))

  private val ada = Person("ada", 36, Address("Wrocław", 50001))

  // ---------------------------------------------------------------- optics

  test("a lens READ inside a direct block — the plainest case, and it works") {
    val out: V[Int] = direct[V]:
      val p = check(ada).reflect
      age.get(p)
    assertEquals(out, Validated.Valid(36))
  }

  test("a lens WRITE inside a direct block, and a composed one") {
    val out: V[Person] = direct[V]:
      val p = check(ada).reflect
      city.set("Kraków")(age.modify(_ + 1)(p))
    assertEquals(out, Validated.Valid(Person("ada", 37, Address("Kraków", 50001))))
  }

  test("a traversal inside a direct block, over a value the block bound") {
    val out: V[Vector[Int]] = direct[V]:
      val p = check(ada).reflect
      each.modify(_ + p.age)(Vector(1, 2, 3))
    assertEquals(out, Validated.Valid(Vector(37, 38, 39)))
  }

  test("two reflects and an optic between them: the block is still applicative") {
    val bad = Person("bob", -1, Address("Nowhere", 0))
    val out: V[Int] = direct[V]:
      val a = check(ada).reflect
      val b = check(bad).reflect
      age.get(a) + age.get(b)
    // both problems, not the first — the optic did not make the block
    // monadic, which is the property worth pinning
    assertEquals(out, Validated.Invalid(Vector("bob: age -1")))
  }

  // ---------------------------------------------------------------- arrows

  test("the Function1 arrow inside a direct block: fanout and split") {
    val A = summon[Optic.Arrow[Function1] & Optic.Traversing[Function1]]
    val out: V[(Int, Int)] = direct[V]:
      val p = check(ada).reflect
      A.fanout((i: Int) => i + 1, (i: Int) => i * 10)(age.get(p))
    assertEquals(out, Validated.Valid((37, 360)))
  }

  test("the Kleisli arrow composed inside a direct block") {
    val K = Optic.kleisliArrow[Option]
    val half = Optic.Star[Option, Int, Int](i => Option.when(i % 2 == 0)(i / 2))
    val out: V[Option[Int]] = direct[V]:
      val p = check(ada).reflect
      K.compose(half, half).run(p.age)
    assertEquals(out, Validated.Valid(Some(9)))   // 36 -> 18 -> 9
  }

  // ---------------------------------------------------------------- the boundary, named

  test("THE ONE REFUSAL: a `.reflect` inside the focus function does not compile") {
    // this is what `ui-direct-example` found, pinned here so the
    // boundary is a test rather than a sentence in a CHANGELOG entry.
    val e = scala.compiletime.testing.typeCheckErrors("""
      import scala.language.implicitConversions
      import okay.*, okay.given
      final case class P(n: Int)
      val n = okay.Lens[P](_.n)
      type V[A] = okay.Validated[Vector[String], A]
      given okay.Semigroup[Vector[String]] with
        def combine(x: Vector[String], y: Vector[String]): Vector[String] = x ++ y
      def one: V[Int] = okay.Validated.Valid(1)
      val out: V[P] = okay.direct[V]:
        n.modify(i => i + one.reflect)(P(0))
      out
    """)
    assert(e.nonEmpty, "a mark under the focus lambda compiled — the recorded boundary has moved")
  }
}
