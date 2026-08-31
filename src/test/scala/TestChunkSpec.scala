package okay

import scala.collection.immutable.ArraySeq

/**
 * `ChunkBuf.tabulate` specializes when it can and stays correct when
 * it cannot — and BOTH halves need a test, because the failure mode
 * of getting this wrong is not a wrong answer but a
 * `ClassCastException` from a value that contradicts its own type.
 * That is exactly what the match-type version did, and what choosing
 * only the backing avoids: `ArraySeq.ofLong` and an `ofRef` of boxed
 * Longs are both honest `ArraySeq[Long]`.
 */
class TestChunkSpec extends munit.FunSuite {

  /** the backing array's runtime class — what specialization means */
  def backing(c: Chunk[?]): String =
    c.asInstanceOf[ArraySeq[?]].unsafeArray.getClass.getSimpleName

  test("a static element type gives an UNBOXED chunk") {
    assertEquals(backing(ChunkBuf.tabulate[Long](8)(_.toLong)), "long[]")
    assertEquals(backing(ChunkBuf.tabulate[Int](8)(identity)), "int[]")
    assertEquals(backing(ChunkBuf.tabulate[Double](8)(_.toDouble)), "double[]")
    assertEquals(backing(ChunkBuf.tabulate[Char](8)(i => ('a' + i).toChar)), "char[]")
    // a reference type has nothing to UNBOX, but the ClassTag still
    // gives a precisely typed array rather than the untyped one
    assertEquals(backing(ChunkBuf.tabulate[String](8)(_.toString)), "String[]")
  }

  test("it composes across inline hops") {
    inline def twice[A](n: Int)(inline f: Int => A): Chunk[A] =
      ChunkBuf.tabulate(n)(f)
    inline def thrice[A](n: Int)(inline f: Int => A): Chunk[A] =
      twice(n)(f)
    assertEquals(backing(thrice[Long](8)(_.toLong)), "long[]")
  }

  test("an abstract element type falls back, and the result is still right") {
    // a NON-inline generic boundary: the ClassTag cannot be summoned,
    // so the boxed backing is used — which is what `Chunks.map` does
    def generic[A](n: Int)(f: Int => A): Chunk[A] = ChunkBuf.tabulate(n)(f)

    val c = generic[Long](8)(_.toLong)
    assertEquals(backing(c), "Object[]", "it specialized where it cannot")
    // and this is the part that matters: the value obeys its type
    assertEquals(c.length, 8)
    assertEquals(c.sum, 28L)
    assertEquals(c.toList, List(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L))
  }

  test("both backings are interchangeable as the same Chunk type") {
    def generic[A](n: Int)(f: Int => A): Chunk[A] = ChunkBuf.tabulate(n)(f)
    val specialized: Chunk[Long] = ChunkBuf.tabulate[Long](8)(_.toLong)
    val fallback: Chunk[Long] = generic[Long](8)(_.toLong)

    assertNotEquals(backing(specialized), backing(fallback), "the probe is vacuous")
    // the same operations, the same answers, no cast anywhere
    assertEquals(specialized.toList, fallback.toList)
    assertEquals(specialized.map(_ * 2).toList, fallback.map(_ * 2).toList)
    assertEquals(specialized ++ fallback, fallback ++ specialized)
  }
}
