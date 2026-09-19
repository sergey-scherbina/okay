package okay

import okay.given
import okay.Optic.{Const, First, Star}
import scala.compiletime.testing.typeCheckErrors

/**
 * Optics on profunctors (specs/optics.md, stage 0): the laws of each
 * family, composition across families typing itself, the effectful
 * traversal in the row, and the two field constructors — on whichever
 * platform compiles this file.
 */
class TestOptics extends munit.FunSuite {

  final case class Address(city: String, zip: Int)
  final case class Person(name: String, age: Int, address: Option[Address])

  sealed trait Shape
  final case class Circle(r: Double) extends Shape
  final case class Square(side: Double) extends Shape

  val rnd = scala.util.Random(20260909)
  def person(): Person =
    Person(rnd.alphanumeric.take(5).mkString, rnd.nextInt(90),
      if rnd.nextBoolean() then Some(Address(rnd.alphanumeric.take(6).mkString, rnd.nextInt(99999))) else None)

  val age: Lens[Person, Person, Int, Int] = Lens[Person](_.age)
  val ageByName = Lens.field[Person]("age")
  val ageByHand: Lens[Person, Person, Int, Int] = Lens(_.age, (p, a) => p.copy(age = a))
  val address: Lens[Person, Person, Option[Address], Option[Address]] = Lens[Person](_.address)
  val zip: Lens[Address, Address, Int, Int] = Lens[Address](_.zip)

  test("the lens laws, on generated values, for all three constructors") {
    for l <- Vector(age, ageByName, ageByHand); _ <- 1 to 100 do
      val p = person(); val v = rnd.nextInt(90); val w = rnd.nextInt(90)
      assertEquals(l.set(l.get(p))(p), p)                // GetPut
      assertEquals(l.get(l.set(v)(p)), v)                // PutGet
      assertEquals(l.set(w)(l.set(v)(p)), l.set(w)(p))   // PutPut
  }

  test("the prism laws: Some, and one case of a hierarchy; a miss leaves modify's argument unchanged") {
    val some = Prism.some[Int, Int]
    for _ <- 1 to 100 do
      val n = rnd.nextInt()
      assertEquals(some.preview(Some(n)), Some(n))                  // preview ∘ review
      assertEquals(some.modify(_ + 1)(None), None)
      assertEquals(some.set(n)(None), None)
    val circle = Prism.of[Shape, Circle]
    assertEquals(circle.preview(Circle(2.0)), Some(Circle(2.0)))
    assertEquals(circle.preview(Square(2.0)), None)
    assertEquals(circle.modify(c => Circle(c.r * 2))(Circle(1.5)), Circle(3.0))
    assertEquals(circle.modify(c => Circle(c.r * 2))(Square(1.5)), Square(1.5))
  }

  test("the traversal laws: identity, composition; toVector lists the foci in order; foldMap combines") {
    val each = Traversal.each[Int, Int]
    for _ <- 1 to 50 do
      val v = Vector.fill(rnd.nextInt(10))(rnd.nextInt(100))
      assertEquals(each.modify(identity)(v), v)
      val f = (n: Int) => n + 3; val g = (n: Int) => n * 2
      assertEquals(each.modify(f.andThen(g))(v), each.modify(f).andThen(each.modify(g))(v))
      assertEquals(each.toVector(v), v)
    given Monoid[Int] with
      def empty = 0
      def combine(x: Int, y: Int) = x + y
    assertEquals(each.foldMap(identity)(Vector(1, 2, 3)), 6)
    assertEquals(Traversal.eachList[Int, Int].modify(_ + 1)(List(1, 2)), List(2, 3))
  }

  test("composition across families types itself: lens ∘ prism ∘ lens is an Affine, no table") {
    val personZip: Affine[Person, Person, Int, Int] = address.andThen(Prism.some).andThen(zip)
    val p = Person("ada", 36, Some(Address("Warszawa", 1)))
    assertEquals(personZip.set(7)(p), p.copy(address = Some(Address("Warszawa", 7))))
    assertEquals(personZip.preview(p), Some(1))
    assertEquals(personZip.set(7)(p.copy(address = None)), p.copy(address = None))   // the miss
    assertEquals(personZip.preview(p.copy(address = None)), None)
    // every pair is accepted by the Function1 interpretation — the meet, by subtyping
    val iso: Iso[Person, Person, Person, Person] = Iso(identity, identity)
    val everyone = Traversal.each[Person, Person]
    assertEquals(iso.andThen(age).modify(_ + 1)(p).age, 37)                                  // iso ∘ lens
    assertEquals(age.andThen(Iso[Int, Int, Int, Int](identity, identity)).modify(_ + 1)(p).age, 37)   // lens ∘ iso
    assertEquals(address.andThen(Prism.some).modify(identity)(p), p)                         // lens ∘ prism
    assertEquals(Prism.some[Address, Address].andThen(zip).modify(_ + 1)(p.address), Some(Address("Warszawa", 2)))   // prism ∘ lens
    assertEquals(personZip.modify(_ + 1)(p).address.map(_.zip), Some(2))                     // affine
    assertEquals(everyone.andThen(age).modify(_ + 1)(Vector(p, p)).map(_.age), Vector(37, 37))
    assertEquals(everyone.andThen(personZip).toVector(Vector(p, p.copy(address = None), p)), Vector(1, 1))
  }

  test("the effectful traversal: Star over a program in the row visits in order and threads the state") {
    val each = Traversal.each[Int, Int]
    // each element is added to the state, and answers the state it saw
    val prog: Vector[Int] ! State % Int =
      each.traverseOf[[X] =>> X ! State % Int]((a: Int) => State.modify[Int](_ + a))(Vector(1, 2, 3))
    val (finalState, seen) = !.run(State.handle(0)(prog))
    assertEquals(finalState, 6)
    assertEquals(seen, Vector(1, 3, 6))
    // through a composed optic too: every person's zip, in an effect
    val zips = Traversal.each[Person, Person].andThen(address).andThen(Prism.some).andThen(zip)
    val people = Vector(Person("a", 1, Some(Address("x", 10))), Person("b", 2, None), Person("c", 3, Some(Address("y", 20))))
    val (count, out) = !.run(State.handle(0)(
      zips.traverseOf[[X] =>> X ! State % Int](z => State.modify[Int](_ + 1).map(_ => z * 2))(people)))
    assertEquals(count, 2)
    assertEquals(out.map(_.address.map(_.zip)), Vector(Some(20), None, Some(40)))
  }

  test("an interpretation's direct road agrees with the derived default it replaces") {
    import Optic.{Strong, Choice, Traversing, Forget}
    // the defaults, spelled out once, against each override
    def defaultLens[P[_, _]](P: Strong[P])(p: P[Int, Int]): P[Person, Person] =
      P.dimap(P.first[Int, Int, Person](p))((s: Person) => (s.age, s), (bs: (Int, Person)) => bs._2.copy(age = bs._1))
    def defaultPrism[P[_, _]](P: Choice[P])(p: P[Int, Int]): P[Option[Int], Option[Int]] =
      P.dimap(P.right[Int, Int, Option[Int]](p))((s: Option[Int]) => s.toRight(None), (e: Either[Option[Int], Int]) => e.fold(identity, Some(_)))
    def defaultEach[P[_, _]](P: Traversing[P])(p: P[Int, Int]): P[Vector[Int], Vector[Int]] = P.wander(Optic.vectorWalk[Int, Int])(p)
    val get: Person => Int = _.age
    val set: (Person, Int) => Person = (p, a) => p.copy(age = a)
    val preview: Option[Int] => Either[Option[Int], Int] = _.toRight(None)
    val review: Int => Option[Int] = Some(_)
    for _ <- 1 to 50 do
      val p = person(); val o = if rnd.nextBoolean() then Some(rnd.nextInt()) else None; val v = Vector.fill(rnd.nextInt(6))(rnd.nextInt())
      val f = (n: Int) => n * 3 + 1
      val F = summon[Traversing[Function1]]
      assertEquals(F.lens(get, set)(f)(p), defaultLens(F)(f)(p))
      assertEquals(F.prism(preview, review)(f)(o), defaultPrism(F)(f)(o))
      assertEquals(F.eachVector(f)(v), defaultEach(F)(f)(v))
      given Monoid[Int] with
        def empty = 0
        def combine(x: Int, y: Int) = x + y
      val G = summon[Traversing[[X, Y] =>> Forget[Int, X, Y]]]
      val fo = Forget[Int, Int, Int](f)
      assertEquals(G.lens(get, set)(fo).run(p), defaultLens(G)(fo).run(p))
      assertEquals(G.prism(preview, review)(fo).run(o), defaultPrism(G)(fo).run(o))
      assertEquals(G.eachVector(fo).run(v), defaultEach(G)(fo).run(v))
      val S = summon[Traversing[[X, Y] =>> Optic.Star[Option, X, Y]]]   // Applicative[Option] is the package's now
      val st = Optic.Star[Option, Int, Int](n => Option.when(n % 2 == 0)(f(n)))
      assertEquals(S.lens(get, set)(st).run(p), defaultLens(S)(st).run(p))
      assertEquals(S.prism(preview, review)(st).run(o), defaultPrism(S)(st).run(o))
  }

  test("compiled: the pair answers what the optic answers, on every family and every operation") {
    val p = Person("ada", 36, Some(Address("Warszawa", 1)))
    val none = p.copy(address = None)
    val personZip: Affine[Person, Person, Int, Int] = address.andThen(Prism.some).andThen(zip)

    // a lens, a prism, an affine, an iso — each compiled once, then
    // asked the same questions as the optic it came from
    val cAge = age.compiled
    assertEquals(cAge.preview(p), age.preview(p))
    assertEquals(cAge.set(7)(p), age.set(7)(p))
    assertEquals(cAge.modify(_ + 1)(p), age.modify(_ + 1)(p))

    val some = Prism.some[Int, Int]
    val cSome = some.compiled
    for o <- Vector(Some(3), None) do
      assertEquals(cSome.preview(o), some.preview(o))
      assertEquals(cSome.set(9)(o), some.set(9)(o))
      assertEquals(cSome.modify(_ * 2)(o), some.modify(_ * 2)(o))

    val cZip = personZip.compiled
    for s <- Vector(p, none) do
      assertEquals(cZip.preview(s), personZip.preview(s))
      assertEquals(cZip.set(7)(s), personZip.set(7)(s))
      assertEquals(cZip.modify(_ + 1)(s), personZip.modify(_ + 1)(s))

    val iso: Iso[Int, Int, Int, Int] = Iso(_ + 1, _ - 1)
    assertEquals(iso.compiled.modify(_ * 2)(10), iso.modify(_ * 2)(10))

    // a TYPE-CHANGING lens compiles too, and the pair carries the change
    val boxed: Lens[(String, Int), (Boolean, Int), String, Boolean] =
      Lens(_._1, (si, b) => (b, si._2))
    assertEquals(boxed.compiled.set(true)(("x", 5)), boxed.set(true)(("x", 5)))

    // generated: every question, on every value, agrees
    for _ <- 1 to 100 do
      val q = person(); val v = rnd.nextInt(90)
      assertEquals(cAge.set(v)(q), age.set(v)(q))
      assertEquals(cZip.set(v)(q), personZip.set(v)(q))
      assertEquals(cZip.preview(q), personZip.preview(q))
  }

  test("compiled is not offered for a traversal: a pair holds one focus, and the type says so") {
    val e = typeCheckErrors("""okay.Traversal.each[Int, Int].compiled""")
    assert(e.nonEmpty, "a traversal was allowed to compile to a one-focus pair")
  }

  test("Lens[S](_.f): the lambda is the getter; not a selector, and no such field, are compile errors") {
    val p = Person("ada", 36, None)
    assertEquals(Lens[Person](_.name).get(p), "ada")
    assertEquals(Lens[Person](_.name).modify(_.toUpperCase)(p), p.copy(name = "ADA"))
    val notASelector = typeCheckErrors("""okay.Lens[Person](_.age + 1)""").map(_.message).mkString
    assert(notASelector.contains("wants a field selector"), notASelector)
    val noSuchField = typeCheckErrors("""okay.Lens[Person](_.agee)""").map(_.message).mkString
    assert(noSuchField.contains("agee"), noSuchField)
    val noSuchName = typeCheckErrors("""okay.Lens.field[Person]("agee")""")
    assert(noSuchName.nonEmpty)
  }
}
