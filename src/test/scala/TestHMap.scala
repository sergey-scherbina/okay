package okay

/** the static heterogeneous map: entries in the type, lookup by the
 * compiler, no cast, a missing key is a compile error */
class TestHMap extends munit.FunSuite {

  final class Key[A](val name: String)

  test("entries live in the type; get resolves at the key's type; a missing key does not compile") {
    val n = Key[Int]("n")
    val s = Key[String]("s")
    val m = HMap.empty[Key].updated(n, 41).updated(s, "x")
    val i: Int = m.get(n)
    val str: String = m.get(s)
    assertEquals(i, 41)
    assertEquals(str, "x")
    assertEquals(m.size, 2)
    val errors = compileErrors("""
      val n = new okay.TestHMap().Key[Int]("n")
      val other = new okay.TestHMap().Key[Int]("other")
      val m = okay.HMap.empty[okay.TestHMap#Key].updated(n, 1)
      m.get(other)""")
    assert(errors.nonEmpty, "a key the map does not hold compiled")
  }

  test("two keys of one type are two entries; the same key added again shadows the older value") {
    val a = Key[Int]("a")
    val b = Key[Int]("b")
    val m = HMap.empty[Key].updated(a, 1).updated(b, 2).updated(a, 3)
    assertEquals(m.get(a), 3)
    assertEquals(m.get(b), 2)
    val (k, v) = m.toTuple.head
    assert(k eq a)
    assertEquals(v, 3)
  }

  test("value keys of one number under two types are two entries, each typed by its key") {
    final case class Id[A](n: Long)
    val s5 = Id[String](5)
    val i5 = Id[Int](5)
    val m = HMap.empty[Id].updated(s5, "five").updated(i5, 5)
    val str: String = m.get(s5)
    val int: Int = m.get(i5)
    assertEquals(str, "five")
    assertEquals(int, 5)
    assertEquals(m.size, 2)
    // HMap keys are the vals' singleton types: an equal but fresh id is not in the type
    val errors = compileErrors("""
      final case class Id[A](n: Long)
      val s5 = Id[String](5)
      val m = okay.HMap.empty[Id].updated(s5, "five")
      m.get(Id[String](5))""")
    assert(errors.nonEmpty, "a fresh equal id found an entry in the static map")
  }
}
