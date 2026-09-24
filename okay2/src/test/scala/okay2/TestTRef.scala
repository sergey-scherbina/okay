package okay2

import java.util.concurrent.atomic.AtomicInteger

/** keys for the typed-key tests, at top level (a class nested in a
 * suite trips -Xlint's outer check) */
object TKeys {
  final class Key[A](val name: String)
  implicit val sameKey: Same[Key] = Same.byIdentity[Key]

  /** a typed id over a primitive: the tag travels with the value */
  final case class TypedId[A](n: Long)(implicit val tag: scala.reflect.ClassTag[A])
  implicit val sameId: Same[TypedId] = Same.byValue[TypedId](
    new Same.Equal[TypedId] { def apply[A, B](a: TypedId[A], b: TypedId[B]): Boolean = a.n == b.n },
    new Same.TagOf[TypedId] { def apply[A](a: TypedId[A]): scala.reflect.ClassTag[A] = a.tag })

  /** a value that is its own stamp, for `TRef.bare` */
  final class Count(val n: Int) extends TRef.Stamped[Count] { def value: Count = this }
}

/** the transactional cell: CAS, versions, the bare road, waiters */
class TestTRef extends munit.FunSuite {
  import TKeys._

  test("modify installs, answers, and moves the version by one per change") {
    val r = TRef(0)
    assertEquals(r.version, 0L)
    assertEquals(r.modify(n => (n + 1, "one")), "one")
    assertEquals(r.get, 1)
    assertEquals(r.version, 1L)
    val _ = r.modify(n => (n, ()))              // a wrapped cell counts every install
    assertEquals(r.version, 2L)
  }

  test("a bare cell: the same object back changes nothing — no version, no wake") {
    val c0 = new Count(0)
    val r = TRef.bare(c0)
    val woke = new AtomicInteger(0)
    r.onChange(() => { woke.incrementAndGet(); () })
    assertEquals(r.modify(c => (c, "same")), "same")
    assertEquals(r.version, 0L)
    assertEquals(woke.get, 0)
    val _ = r.modify(c => (new Count(c.n + 1), ()))
    assertEquals(r.get.n, 1)
    assertEquals(r.version, 1L)
    assertEquals(woke.get, 1)
  }

  test("waiters fire once, at the next change, in the order they registered") {
    val r = TRef("a")
    val seen = scala.collection.mutable.ListBuffer.empty[String]
    r.onChange(() => { seen += "first"; () })
    r.onChange(() => { seen += "second"; () })
    val _ = r.modify(_ => ("b", ()))
    val _ = r.modify(_ => ("c", ()))
    assertEquals(seen.toList, List("first", "second"))
  }

}

/** the heterogeneous map with typed keys: the key's type is the value's,
 * keys are identities, iteration is typed — the Scala 3 core's TestTMap */
class TestTMap extends munit.FunSuite {
  import TKeys._

  test("a key holds a value of its own type; a wrong type does not compile") {
    val n = new Key[Int]("n")
    val s = new Key[String]("s")
    val m = TMap.empty[Key].updated(n, 41).updated(s, "x")
    val got: Option[Int] = m.get(n)
    assertEquals(got, Some(41))
    assertEquals(m.get(s), Some("x"))
    assertEquals(m.size, 2)
    val errors = compileErrors("""
      val k = new okay2.TKeys.Key[Int]("k")
      okay2.TMap.empty[okay2.TKeys.Key].updated(k, "not an int")""")
    assert(errors.nonEmpty, "a String under an Int key compiled")
  }

  test("keys are identities: an equal-but-distinct key is another entry; the same key is replaced in place") {
    val a = new Key[Int]("same")
    val b = new Key[Int]("same")
    val m = TMap.empty[Key].updated(a, 1).updated(b, 2).updated(a, 3)
    assertEquals(m.get(a), Some(3))
    assertEquals(m.get(b), Some(2))
    assertEquals(m.size, 2)
    assertEquals(m.entries.map(_.key.name).toList, List("same", "same"))
    assertEquals(m.get(new Key[Int]("same")), None)
  }

  test("typed iteration: the polymorphic function sees each value at its key's type, in insertion order") {
    val n = new Key[Int]("n")
    val s = new Key[String]("s")
    val m = TMap.empty[Key].updated(n, 2).updated(s, "ab")
    val seen = scala.collection.mutable.ListBuffer.empty[String]
    m.foreach(new TMap.Each[Key] { def apply[A](k: Key[A], v: A): Unit = { seen += s"${k.name}=$v"; () } })
    assertEquals(seen.toList, List("n=2", "s=ab"))
  }
}

/** the sameness typeclass: a witness when the tokens are one — the
 * Scala 3 core's TestSame, less its strictEquality test (Scala 2 has no
 * strict equality to derive `CanEqual` for) */
class TestSame extends munit.FunSuite {
  import TKeys._

  test("the same token yields A =:= B; a different token, even an equal-looking one, yields nothing") {
    val a = new Key[Int]("k")
    val b = new Key[Int]("k")
    assert(a.sameAs(a).isDefined)
    assert(a.sameAs(b).isEmpty)
    val v: Int = 41
    val w: Option[Int] = a.sameAs(a).map(ev => ev(v))
    assertEquals(w, Some(41))
  }

  test("a value key: equal value and equal tag is the same key; equal value under another type is not") {
    val s5 = TypedId[String](5)
    val s5again = TypedId[String](5)
    val i5 = TypedId[Int](5)
    val s6 = TypedId[String](6)
    assert(s5.sameAs(s5again).isDefined, "two equal ids of one type are one key")
    assert(s5.sameAs(i5).isEmpty, "equal numbers under different types are different keys")
    assert(s5.sameAs(s6).isEmpty)
    val m = TMap.empty[TypedId].updated(s5, "five").updated(i5, 5).updated(s5again, "five again")
    val str: Option[String] = m.get(s5)
    val int: Option[Int] = m.get(i5)
    assertEquals(str, Some("five again"))
    assertEquals(int, Some(5))
    assertEquals(m.size, 2)
  }

  test("=== is the witness, =!= the boolean: in the Some branch the compiler knows A is B") {
    val a = new Key[Int]("a")
    val b = new Key[Int]("b")
    def moveValue[A, B](from: Key[A], to: Key[B], v: A): Option[B] =
      (from === to).map(ev => ev(v))     // an A becomes a B only with the proof in hand
    assertEquals(moveValue(a, a, 7), Some(7))
    assertEquals(moveValue(a, b, 7), None)
    assert(a =!= b)
    assert(!(a =!= a))
  }
}
