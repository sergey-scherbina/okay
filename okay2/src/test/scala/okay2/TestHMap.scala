package okay2

/** HMap (spec stage 20): the Scala 3 core's TestHMap — entries in the
 * type, lookup by the compiler, no cast, a missing key is a compile error */
class TestHMap extends munit.FunSuite {
  import TestHMap._

  test("HMap: entries live in the type; get resolves at the key's type; a missing key does not compile") {
    val n = new Key[Int]("n")
    val s = new Key[String]("s")
    val m = HMap.empty[Key].updated(n, 41).updated(s, "x")
    val i: Int = m.get(n)
    val str: String = m.get(s)
    assertEquals(i, 41)
    assertEquals(str, "x")
    assertEquals(m.size, 2)
    val errors = compileErrors("""
      val n = new okay2.TestHMap.Key[Int]("n")
      val other = new okay2.TestHMap.Key[Int]("other")
      val m = okay2.HMap.empty[okay2.TestHMap.Key].updated(n, 1)
      m.get(other)""")
    assert(errors.contains("has no entry for the key"), errors)
  }

  test("HMap: two keys of one type are two entries; the same key added again shadows the older value") {
    val a = new Key[Int]("a")
    val b = new Key[Int]("b")
    val m = HMap.empty[Key].updated(a, 1).updated(b, 2).updated(a, 3)
    assertEquals(m.get(a), 3)
    assertEquals(m.get(b), 2)
    val HMap.Cons(k, v, _) = m.toList
    assert(k eq a)
    assertEquals(v, 3)
    assertEquals(m.size, 3)
  }

  test("HMap: value keys of one number under two types are two entries; an equal fresh key is not in the type") {
    val s5 = Num[String](5)
    val i5 = Num[Int](5)
    val m = HMap.empty[Num].updated(s5, "five").updated(i5, 5)
    val str: String = m.get(s5)
    val int: Int = m.get(i5)
    assertEquals(str, "five")
    assertEquals(int, 5)
    val errors = compileErrors("""
      val s5 = okay2.TestHMap.Num[String](5)
      val m = okay2.HMap.empty[okay2.TestHMap.Num].updated(s5, "five")
      m.get(okay2.TestHMap.Num[String](5))""")
    assert(errors.nonEmpty, "a fresh equal id found an entry in the static map")
  }
}

object TestHMap {
  final class Key[A](val name: String)
  final case class Num[A](n: Long)
}
