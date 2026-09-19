package okay

import scala.collection.immutable.ArraySeq

/**
 * `ChunkBuf` specializes when it can and stays correct when
 * it cannot — and BOTH halves need a test, because the failure mode
 * of getting this wrong is not a wrong answer but a
 * `ClassCastException` from a value that contradicts its own type.
 * That is exactly what the match-type version did, and what choosing
 * only the backing avoids: `ArraySeq.ofLong` and an `ofRef` of boxed
 * Longs are both honest `ArraySeq[Long]`.
 */
class TestChunkSpec extends munit.FunSuite {

  /** fill a buffer by index — what `tabulate` used to be, now just
   * what `ChunkBuf` is, since `apply` specializes on its own */
  inline def buildWith[A](n: Int)(inline f: Int => A): Chunk[A] =
    val b = ChunkBuf[A](n)
    var i = 0
    while i < n do
      b(i) = f(i)
      i += 1
    b.chunk

  /** the backing array's runtime class — what specialization means */
  def backing(c: Chunk[?]): String =
    c.asInstanceOf[ArraySeq[?]].unsafeArray.getClass.getSimpleName

  test("a static element type gives an UNBOXED chunk") {
    assertEquals(backing(buildWith[Long](8)(_.toLong)), "long[]")
    assertEquals(backing(buildWith[Int](8)(identity)), "int[]")
    assertEquals(backing(buildWith[Double](8)(_.toDouble)), "double[]")
    assertEquals(backing(buildWith[Char](8)(i => ('a' + i).toChar)), "char[]")
    // a reference type has nothing to UNBOX, but the ClassTag still
    // gives a precisely typed array rather than the untyped one
    assertEquals(backing(buildWith[String](8)(_.toString)), "String[]")
  }

  test("it composes across inline hops") {
    inline def twice[A](n: Int)(inline f: Int => A): Chunk[A] =
      buildWith(n)(f)
    inline def thrice[A](n: Int)(inline f: Int => A): Chunk[A] =
      twice(n)(f)
    assertEquals(backing(thrice[Long](8)(_.toLong)), "long[]")
  }

  test("an abstract element type falls back, and the result is still right") {
    // a NON-inline generic boundary: the ClassTag cannot be summoned,
    // so the boxed backing is used — which is what `Chunks.map` does
    def generic[A](n: Int)(f: Int => A): Chunk[A] = buildWith(n)(f)

    val c = generic[Long](8)(_.toLong)
    assertEquals(backing(c), "Object[]", "it specialized where it cannot")
    // and this is the part that matters: the value obeys its type
    assertEquals(c.length, 8)
    assertEquals(c.sum, 28L)
    assertEquals(c.toList, List(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L))
  }

  test("both backings are interchangeable as the same Chunk type") {
    def generic[A](n: Int)(f: Int => A): Chunk[A] = buildWith(n)(f)
    val specialized: Chunk[Long] = buildWith[Long](8)(_.toLong)
    val fallback: Chunk[Long] = generic[Long](8)(_.toLong)

    assertNotEquals(backing(specialized), backing(fallback), "the probe is vacuous")
    // the same operations, the same answers, no cast anywhere
    assertEquals(specialized.toList, fallback.toList)
    assertEquals(specialized.map(_ * 2).toList, fallback.map(_ * 2).toList)
    assertEquals(specialized ++ fallback, fallback ++ specialized)
  }

  test("the whole producer family specializes, not just tabulate") {
    // generate: the filler is built once and drives the recursion, so
    // EVERY chunk is unboxed, not only the first
    val gen = Chunks.generate(0L)(identity)(_ + 1)(size = 8)
    val firstTwo = Chunks.take(gen)(16)
    val chunks = collect(firstTwo)
    assert(chunks.length >= 2, s"only ${chunks.length} chunks to judge by")
    for c <- chunks do assertEquals(backing(c), "long[]")

    // map over an unboxed source stays unboxed
    val mapped = collect(Chunks.take(Chunks.map(gen)(_ * 2))(16))
    for c <- mapped do assertEquals(backing(c), "long[]")

    // filter too
    val kept = collect(Chunks.take(Chunks.filter(gen)(_ % 2 == 0))(4))
    for c <- kept do assertEquals(backing(c), "long[]")

    // and `of`, which is what the interop modules hand over
    assertEquals(backing(ChunkBuf.ofSpecialized(Vector(1L, 2L, 3L))), "long[]")
  }

  test("a generic caller still gets correct answers from all of them") {
    // no ClassTag reachable: every one of these falls back, and the
    // values must still be right — that is the whole safety claim
    def genMap[A, B](p: Chunks[A])(f: A => B): Chunks[B] =
      Chunks.mapWith(p)(ChunkBuf.mapper[A, B](f))

    val gen = Chunks.generate(0L)(identity)(_ + 1)(size = 8)
    val out = collect(Chunks.take(genMap(gen)((x: Long) => x * 3))(16))
    for c <- out do assertEquals(backing(c), "Object[]", "it specialized where it cannot")
    assertEquals(out.flatMap(_.toList).take(8), List(0L, 3L, 6L, 9L, 12L, 15L, 18L, 21L))
  }

  /** the chunks a producer yields, as a list */
  def collect[A](p: Chunks[A]): List[Chunk[A]] =
    def go(x: Chunks[A], acc: List[Chunk[A]]): List[Chunk[A]] =
      Chunks.pull(x) match
        case None => acc.reverse
        case Some((c, r)) => go(r, c :: acc)
    go(p, Nil)

  test("a REIFIED pipeline specializes too: the tag rides with the existential") {
    // this was the wall. `Pipeline.chunks` compiles a `Mapped` node
    // whose intermediate type is existential, so no ClassTag could be
    // summoned there — the node now carries the one captured where it
    // was built, and the compiled chunks come out unboxed.
    val p = Pipeline.range(0L, 64L, 8).map(_ * 2)
    val chunks = collect(Pipeline.chunks(p))
    assert(chunks.nonEmpty)
    for c <- chunks do assertEquals(backing(c), "long[]")
  }

  test("and it survives the optimizer, which rebuilds the nodes") {
    // map/map fusion reconstructs Mapped; the tag must come along or
    // the optimized tree quietly loses the specialization
    val p = Pipeline.range(0L, 64L, 8).map(_ * 2).map(_ + 1)
    val optimized = Pipeline.optimize(p)
    assert(Pipeline.depth(optimized) < Pipeline.depth(p), "nothing was fused")
    for c <- collect(Pipeline.chunks(optimized)) do
      assertEquals(backing(c), "long[]")
    // and the answers are unchanged by any of it
    assertEquals(Pipeline.fold(p)(using Fold.sum[Long]),
      (0L until 64L).map(_ * 2 + 1).sum)
  }

  test("the buffer that survives across pulls specializes too") {
    // fromIterator and rechunk fill from an external source a pull at
    // a time, so `tabulate` cannot reach them — TaggedBuf can, because
    // its backing type is existential and claims nothing
    val fromIt = collect(Chunks.fromIterator((1L to 200L).iterator, 64))
    assertEquals(fromIt.flatMap(_.toList), (1L to 200L).toList)
    for c <- fromIt do assertEquals(backing(c), "long[]")

    val re = collect(Chunks.rechunk(Chunks.range(0L, 100L, 7))(32))
    assertEquals(re.flatMap(_.toList), (0L until 100L).toList)
    for c <- re do assertEquals(backing(c), "long[]")
  }

  test("and it is SOUND generically, which the type-level attempts were not") {
    // a generically built buffer, statically a TaggedBuf[Long]: this
    // is the exact shape that crashed `opaque ChunkBuf[A] = Array[A]`
    // and the match type with a ClassCastException
    def generic[A](n: Int)(f: Int => A): Chunk[A] =
      val b = ChunkBuf.boxed[A](n)
      var i = 0
      while i < n do { b(i) = f(i); i += 1 }
      b.chunk

    val c: Chunk[Long] = generic[Long](8)(_.toLong)
    assertEquals(backing(c), "Object[]", "it specialized where it cannot")
    assertEquals(c.sum, 28L)
    assertEquals(c.toList, (0L until 8L).toList)
  }

  test("a short final chunk keeps its backing") {
    // take(n) must copy into an array of the SAME component type, or
    // the tail of every stream quietly falls back to boxed
    val chunks = collect(Chunks.fromIterator((1L to 10L).iterator, 4))
    assertEquals(chunks.map(_.length), List(4, 4, 2))
    for c <- chunks do assertEquals(backing(c), "long[]")
  }
}
