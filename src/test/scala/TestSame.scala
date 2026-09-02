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
}
