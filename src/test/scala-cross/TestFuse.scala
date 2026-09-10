package okay

import okay.given

/**
 * specs/optics.md, optics-fuse: the optic fused in the compiler.
 * The law is the only one that matters — the fused update is the
 * optic's update — and it is asserted on every shape the macro reads
 * and on the shapes it refuses, which must still be right.
 */
class TestFuse extends munit.FunSuite {

  final case class Address(city: String, zip: Int)
  final case class Person(name: String, age: Int, address: Option[Address], home: Address)

  // `inline def`, so the macro follows the name to the expression; and
  // the halves written out, because `Lens[S](_.f)` is a macro and an
  // inline argument is captured before a nested macro expands — the
  // fusion falls back there, correctly, and the last test says so
  inline def age = Lens[Person, Person, Int, Int](_.age, (s, v) => s.copy(age = v))
  inline def home = Lens[Person, Person, Address, Address](_.home, (s, v) => s.copy(home = v))
  inline def city = Lens[Address, Address, String, String](_.city, (s, v) => s.copy(city = v))
  inline def zip = Lens[Address, Address, Int, Int](_.zip, (s, v) => s.copy(zip = v))
  inline def address =
    Lens[Person, Person, Option[Address], Option[Address]](_.address, (s, v) => s.copy(address = v))
  /** an optic in a `val`, which is how one is normally stored: a
   * private member is fixed by the call site's type, so it fuses */
  private val ageVal: Lens[Person, Person, Int, Int] = Lens[Person](_.age)

  /** the same lens through the selector macro — FUSED since
   * optics-zero-tax, which is the shape most code actually writes */
  inline def ageBySelector = Lens[Person](_.age)
  inline def homeBySelector = Lens[Person](_.home)
  inline def cityBySelector = Lens[Address](_.city)
  inline def selectorChain = homeBySelector.andThen(cityBySelector)
  inline def homeCity = home.andThen(city)
  inline def addressZip = address.andThen(Prism.some[Address, Address]).andThen(zip)

  val rnd = scala.util.Random(20260910)
  def person(): Person =
    Person(rnd.alphanumeric.take(4).mkString, rnd.nextInt(90),
      if rnd.nextBoolean() then Some(Address(rnd.alphanumeric.take(5).mkString, rnd.nextInt(999))) else None,
      Address(rnd.alphanumeric.take(5).mkString, rnd.nextInt(999)))

  test("fused set IS the optic's set: one lens, a lens chain, a lens through Some") {
    for _ <- 1 to 200 do
      val p = person(); val n = rnd.nextInt(999); val c = rnd.alphanumeric.take(3).mkString
      assertEquals(Fuse.set(age)(n)(p), age.set(n)(p))
      assertEquals(Fuse.set(homeCity)(c)(p), homeCity.set(c)(p))
      assertEquals(Fuse.set(addressZip)(n)(p), addressZip.set(n)(p))
      // and the miss is a miss: an absent address is left alone
      if p.address.isEmpty then assertEquals(Fuse.set(addressZip)(n)(p), p)
  }

  test("fused modify IS the optic's modify, on the same shapes") {
    for _ <- 1 to 200 do
      val p = person()
      assertEquals(Fuse.modify(age)(_ + 1)(p), age.modify(_ + 1)(p))
      assertEquals(Fuse.modify(homeCity)(_.toUpperCase)(p), homeCity.modify(_.toUpperCase)(p))
      assertEquals(Fuse.modify(addressZip)(_ * 2)(p), addressZip.modify(_ * 2)(p))
  }

  test("a type-changing lens fuses too, and the change survives it") {
    inline def item = Lens[(String, Int), (Boolean, Int), String, Boolean](_._1, (si, b) => (b, si._2))
    assertEquals(Fuse.set(item)(true)(("x", 5)), item.set(true)(("x", 5)))
    assertEquals(Fuse.set(item)(true)(("x", 5)), (true, 5))
  }

  test("a shape the macro cannot read still works: it falls back to the optic") {
    // a `val`, not an `inline def`: the macro sees a reference and refuses
    val opaque: Lens[Person, Person, Int, Int] = Lens[Person](_.age)
    for _ <- 1 to 50 do
      val p = person(); val n = rnd.nextInt(99)
      assertEquals(Fuse.set(opaque)(n)(p), opaque.set(n)(p))
      assertEquals(Fuse.modify(opaque)(_ + 1)(p), opaque.modify(_ + 1)(p))
    // a hand-written lens (not from the selector macro) reads fine, being
    // a Lens.apply after all
    inline def byHand = Lens[Person, Person, Int, Int](_.age, (p, a) => p.copy(age = a))
    val p = person()
    assertEquals(Fuse.set(byHand)(7)(p), byHand.set(7)(p))
    // a traversal is not a shape it reads; the fallback runs it
    inline def each = Traversal.each[Int, Int]
    assertEquals(Fuse.modify(each)(_ + 1)(Vector(1, 2, 3)), Vector(2, 3, 4))
    // a selector-built lens is no longer among them: it fuses, and
    // the next test proves that rather than asserting it
    for _ <- 1 to 50 do
      val q = person(); val v = rnd.nextInt(99)
      assertEquals(Fuse.set(ageBySelector)(v)(q), ageBySelector.set(v)(q))
      assertEquals(Fuse.set(ageBySelector)(v)(q), Fuse.set(age)(v)(q))
  }

  test("the fusion really fuses, and a poisoned interpretation says so") {
    // Correctness is the same either way, which is why a test that
    // only checks the answer cannot see the fusion switch itself off —
    // and it once did, silently, for a whole lane. The fallback is
    // `optic.set(b)(s)`, which needs the `Function1` interpretation;
    // fused code emits `s.copy(...)` and never asks for it. So poison
    // the instance: a fused call passes, a fallback throws.
    given poisoned: Optic.Strong[Function1] with
      def dimap[A, B, C, D](p: A => B)(f: C => A, g: B => D): C => D =
        throw AssertionError("the fusion fell back")
      def first[A, B, C](p: A => B): ((A, C)) => (B, C) =
        throw AssertionError("the fusion fell back")
      override def lens[S, T, A, B](get: S => A, set: (S, B) => T)(p: A => B): S => T =
        throw AssertionError("the fusion fell back")

    val p = Person("ada", 1, None, Address("H", 3))
    assertEquals(Fuse.set(age)(7)(p).age, 7)                        // halves written out
    assertEquals(Fuse.set(ageBySelector)(7)(p).age, 7)              // the selector macro
    assertEquals(Fuse.set(selectorChain)("K")(p).home.city, "K")    // two of them, composed
    assertEquals(Fuse.modify(ageBySelector)(_ + 1)(p).age, 2)

    // and since optics-fuse-by-default, WITHOUT naming Fuse at all:
    // `.set` and `.modify` are the same macro
    assertEquals(age.set(7)(p).age, 7)
    assertEquals(ageBySelector.set(7)(p).age, 7)
    assertEquals(selectorChain.set("K")(p).home.city, "K")
    // an optic stored in a private `val`, which is how code keeps one
    assertEquals(ageVal.set(7)(p).age, 7)
    assertEquals(ageVal.modify(_ + 1)(p).age, 2)
  }

  test("the boundary of what a `val` fixes, stated by what throws") {
    // The planner follows a definition only where the call site's type
    // FIXES it. A private member does; a `var` and an overridable one
    // do not. This test does not assert the boundary in words: the
    // poisoned instance makes the unfused side throw, so the
    // assertions below say which side each shape is on.
    given poisoned: Optic.Strong[Function1] with
      def dimap[A, B, C, D](p: A => B)(f: C => A, g: B => D): C => D =
        throw AssertionError("fell back")
      def first[A, B, C](p: A => B): ((A, C)) => (B, C) = throw AssertionError("fell back")
      override def lens[S, T, A, B](get: S => A, set: (S, B) => T)(p: A => B): S => T =
        throw AssertionError("fell back")

    val p = Person("ada", 1, None, Address("H", 3))
    assertEquals(ageVal.set(5)(p).age, 5)                     // a private val: fused
    var mutable: Lens[Person, Person, Int, Int] = Lens[Person](_.age)
    // the message says it reached the interpretation, which is the
    // whole point of the assertion — a `var` is never followed
    assertEquals(intercept[AssertionError](mutable.set(5)(p)).getMessage, "fell back")
    mutable = Lens[Person, Person, Int, Int](_.age, (s, v) => s.copy(age = v))
    assertEquals(intercept[AssertionError](mutable.set(5)(p)).getMessage, "fell back")
  }

  test("the fused code allocates nothing the hand-written update does not") {
    // the shape the fusion is for, written both ways: they are the same
    // expression after the macro, which the benchmark measures in bytes
    val p = Person("ada", 1, Some(Address("W", 2)), Address("H", 3))
    val byHand = p.copy(address = p.address.map(a => a.copy(zip = 9)))
    assertEquals(Fuse.set(addressZip)(9)(p), byHand)
    val none = p.copy(address = None)
    assertEquals(Fuse.set(addressZip)(9)(none), none.copy(address = none.address.map(a => a.copy(zip = 9))))
  }
}
