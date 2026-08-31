package okay

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

/**
 * What an existential package with a ClassTag can and cannot do —
 * both halves, because the useful answer is where the line is.
 */
class TestTagged extends munit.FunSuite {

  test("it turns an unchecked cast into a checked one") {
    val t = Tagged(42)
    assertEquals(t.as[Int], Some(42))
    assertEquals(t.as[String], None, "a wrong guess must not succeed")
    // and it says what was actually there, which asInstanceOf cannot
    assertEquals(Tagged.expect[String](t), Left("expected java.lang.String, got int"))
  }

  test("heterogeneous storage, recovered safely — what it is FOR") {
    // the shape a continuation frame or a reified node has: values of
    // different types in one collection, each read back at the type
    // that was known when it went in
    val store: List[Tagged] = List(Tagged(1), Tagged("two"), Tagged(3.0))
    assertEquals(store.flatMap(_.as[Int]), List(1))
    assertEquals(store.flatMap(_.as[String]), List("two"))
    assertEquals(store.flatMap(_.as[Double]), List(3.0))
    // nothing was mis-read, and nothing threw
    assertEquals(store.map(_.typeName).map(_.split('.').last),
      List("int", "String", "double"))
  }

  test("what it CANNOT do: erasure is still erasure") {
    // a ClassTag knows the class, and a class does not know its type
    // arguments — so this cannot distinguish a chunk of Ints from a
    // chunk of Strings, which is exactly what the Chunks casts would
    // need it to do
    val ints: Chunk[Int] = ChunkBuf.tabulate[Int](3)(identity)
    val strs: Chunk[String] = ChunkBuf.tabulate[String](3)(_.toString)
    val ti = Tagged(ints)
    val ts = Tagged(strs)

    // A ClassTag names a CLASS, and a class does not carry its type
    // arguments — so both of these are simply "ArraySeq", even though
    // one is backed by int[] and the other by String[]. The tag is
    // taken from the static type, not from the value.
    assertEquals(ti.typeName, ts.typeName)
    assertEquals(ti.typeName, "scala.collection.immutable.ArraySeq")

    // and therefore `as` says yes to the WRONG element type
    assert(ti.as[Chunk[String]].isDefined,
      "the tag distinguished type arguments — erasure has changed")
  }

  test("the invariant kind: nothing to check against") {
    // in `Chunks.mapWith` the chunk's type is known because the
    // program is a Chunks[A] — not because anything about the value
    // says so. A tag would confirm "an ArraySeq" and stop there, which
    // is the check that was never in doubt.
    val p: Chunks[Int] = Chunks.fromIterator(Iterator(1, 2, 3), 2)
    val doubled = Chunks.map(p)(_ * 2)
    assertEquals(Chunks.toLazyList(doubled).flatMap(_.toList).toList, List(2, 4, 6))
  }
}
