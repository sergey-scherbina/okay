package okay

/** the heterogeneous map with typed keys: the key's type is the
 * value's, keys are identities, iteration is typed */
class TestTMap extends munit.FunSuite {

  final class Key[A](val name: String)

  test("a key holds a value of its own type; a wrong type does not compile") {
    val n = Key[Int]("n")
    val s = Key[String]("s")
    val m = TMap.empty[Key].updated(n, 41).updated(s, "x")
    val got: Option[Int] = m.get(n)
    assertEquals(got, Some(41))
    assertEquals(m.get(s), Some("x"))
    assertEquals(m.size, 2)
    val errors = compileErrors("""
      val k = new okay.TestTMap().Key[Int]("k")
      okay.TMap.empty[okay.TestTMap#Key].updated(k, "not an int")""")
    assert(errors.nonEmpty, "a String under an Int key compiled")
  }

  test("keys are identities: an equal-but-distinct key is another entry; the same key is replaced in place") {
    val a = Key[Int]("same")
    val b = Key[Int]("same")
    val m = TMap.empty[Key].updated(a, 1).updated(b, 2).updated(a, 3)
    assertEquals(m.get(a), Some(3))
    assertEquals(m.get(b), Some(2))
    assertEquals(m.size, 2)
    assertEquals(m.entries.map(_.key.name).toList, List("same", "same"))
    assertEquals(m.get(Key[Int]("same")), None)
  }

  test("typed iteration: the polymorphic function sees each value at its key's type, in insertion order") {
    val n = Key[Int]("n")
    val s = Key[String]("s")
    val m = TMap.empty[Key].updated(n, 2).updated(s, "ab")
    val seen = scala.collection.mutable.ListBuffer.empty[String]
    m.foreach([A] => (k: Key[A], v: A) => seen += s"${k.name}=${v}")
    assertEquals(seen.toList, List("n=2", "s=ab"))
    // and a typed use of the value inside: lengths and doubles without a cast
    var total = 0
    m.foreach([A] => (k: Key[A], v: A) => v match
      case i: Int => total += i * 10
      case str: String => total += str.length
      case _ => ())
    assertEquals(total, 22)
  }
}
