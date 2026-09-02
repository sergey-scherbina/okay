package okay

/** the sameness typeclass: a witness when the tokens are one, and
 * strict equality that follows from it */
class TestSame extends munit.FunSuite {

  final class Key[A](val name: String)
  given Same[Key] = Same.byIdentity

  test("the same token yields A =:= B; a different token, even an equal-looking one, yields nothing") {
    val a = Key[Int]("k")
    val b = Key[Int]("k")
    assert(a.sameAs(a).isDefined)
    assert(a.sameAs(b).isEmpty)
    // the witness converts: an Int under `a` is an Int under `a`
    val v: Int = 41
    val w: Option[Int] = a.sameAs(a).map(ev => ev(v))
    assertEquals(w, Some(41))
  }

  test("strict equality: keys of one constructor compare with ==; a key and a String do not compile") {
    import scala.language.strictEquality
    val a = Key[Int]("a")
    val s = Key[String]("s")
    assert(a == a)
    assert(!(a == s))
    val errors = compileErrors("""
      import scala.language.strictEquality
      val t = new okay.TestSame()
      val a = t.Key[Int]("a")
      a == "a" """)
    assert(errors.nonEmpty, "a key compared to a String compiled under strictEquality")
  }

  /** a typed id over a primitive: the tag travels with the value */
  final case class Id[A](n: Long)(using val tag: scala.reflect.ClassTag[A])
  given Same[Id] = Same.byValue([A, B] => (a: Id[A], b: Id[B]) => a.n == b.n, [A] => (a: Id[A]) => a.tag)

  test("a value key: equal value and equal tag is the same key; equal value under another type is not") {
    val s5 = Id[String](5)
    val s5again = Id[String](5)
    val i5 = Id[Int](5)
    val s6 = Id[String](6)
    assert(s5.sameAs(s5again).isDefined, "two equal ids of one type are one key")
    assert(s5.sameAs(i5).isEmpty, "equal numbers under different types are different keys")
    assert(s5.sameAs(s6).isEmpty)
    // and through TMap: the same key replaces, the other type stands beside it, each at its own type
    val m = TMap.empty[Id].updated(s5, "five").updated(i5, 5).updated(s5again, "five again")
    val str: Option[String] = m.get(s5)
    val int: Option[Int] = m.get(i5)
    assertEquals(str, Some("five again"))
    assertEquals(int, Some(5))
    assertEquals(m.size, 2)
  }
}
