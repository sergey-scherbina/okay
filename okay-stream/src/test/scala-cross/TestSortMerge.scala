package okay


import okay.freer.*


import okay.std.*
import Chunks.elements
import scala.util.Random

/**
 * The sort-merge join by key (specs/stream-join.md): the machine
 * driven over two lists — the reference driver, a dozen lines — then
 * the chunk driver against it and against `Bulk.local`'s hash join.
 */
class TestSortMerge extends munit.FunSuite with okay.testkit.Munit.Diagnosed {
  import SortMerge.Need

  /** the reference driver: the machine over two lists, and the most it held */
  private def drive[K, A, B, O](m: SortMerge[K, A, B, O], l: List[(K, A)], r: List[(K, B)]): (Vector[O], Int) =
    val out = Vector.newBuilder[O]
    var ls = l; var rs = r; var maxHeld = 0
    var done = false
    while !done do
      m.step(o => { out += o; () }) match
        case Need.Done => done = true
        case Need.Left => ls match
          case (k, a) :: t => m.left(k, a); ls = t
          case Nil => m.leftEnd()
        case Need.Right => rs match
          case (k, b) :: t => m.right(k, b); rs = t
          case Nil => m.rightEnd()
      maxHeld = maxHeld max m.held
    (out.result(), maxHeld)

  private val bulk = Bulk.local(_ => Iterator.empty)
  private def hashJoin[K, A, B](l: List[(K, A)], r: List[(K, B)]): Vector[(K, (A, B))] =
    bulk.join(bulk.of(l), bulk.of(r)).elements.toVector

  /** n rows over `keys` keys, sorted, each row tagged by its position */
  private def sortedRows(rnd: Random, n: Int, keys: Int, tag: String): List[(Int, String)] =
    List.fill(n)(rnd.nextInt(keys)).sorted.zipWithIndex.map((k, i) => (k, s"$tag$i"))

  private val l3 = List((1, "a"), (2, "b"), (4, "d"))
  private val r4 = List((2, "x"), (3, "y"), (3, "z"), (4, "w"))

  test("the inner join is the hash join's answer, in the hash join's order, on random sorted runs") {
    for seed <- 1 to 30 do
      val rnd = Random(seed)
      val l = sortedRows(rnd, rnd.nextInt(60), 1 + rnd.nextInt(12), "l")
      val r = sortedRows(rnd, rnd.nextInt(60), 1 + rnd.nextInt(12), "r")
      note(s"seed $seed: ${l.size} x ${r.size}")
      val (out, _) = drive(SortMerge.inner[Int, String, String], l, r)
      assertEquals(out, hashJoin(l, r), s"seed $seed")
  }

  test("a run of m left and n right rows produces m x n pairs, left-major, holding n rows") {
    val l = List((5, "a"), (5, "b"), (5, "c"))
    val r = List((5, "x"), (5, "y"), (5, "z"), (5, "w"))
    val (out, held) = drive(SortMerge.inner[Int, String, String], l, r)
    assertEquals(out, (for a <- l; b <- r yield (5, (a._2, b._2))).toVector)
    assertEquals(held, 4, "more than the right run was held")
    // a long left run against a short right one holds the right one only
    val (out2, held2) = drive(SortMerge.inner[Int, Int, String], List.tabulate(10000)(i => (7, i)), List((7, "x"), (7, "y")))
    assertEquals(out2.size, 20000)
    assertEquals(held2, 2)
  }

  test("the unmatched side per variant: inner drops, left answers None on the right, full on either") {
    assertEquals(drive(SortMerge.inner[Int, String, String], l3, r4)._1,
      Vector((2, ("b", "x")), (4, ("d", "w"))))
    assertEquals(drive(SortMerge.left[Int, String, String], l3, r4)._1,
      Vector((1, ("a", None)), (2, ("b", Some("x"))), (4, ("d", Some("w")))))
    assertEquals(drive(SortMerge.full[Int, String, String], l3, r4)._1,
      Vector((1, (Some("a"), None)), (2, (Some("b"), Some("x"))),
             (3, (None, Some("y"))), (3, (None, Some("z"))), (4, (Some("d"), Some("w")))))
    // a right run past the left's end is told by full when released, dropped by the others
    val r5 = r4 :+ (7, "q")
    assertEquals(drive(SortMerge.inner[Int, String, String], l3, r5)._1, drive(SortMerge.inner[Int, String, String], l3, r4)._1)
    assertEquals(drive(SortMerge.left[Int, String, String], l3, r5)._1, drive(SortMerge.left[Int, String, String], l3, r4)._1)
    assertEquals(drive(SortMerge.full[Int, String, String], l3, r5)._1.last, (7, (None, Some("q"))))
  }

  test("an empty side") {
    assertEquals(drive(SortMerge.inner[Int, String, String], l3, Nil)._1, Vector.empty)
    assertEquals(drive(SortMerge.inner[Int, String, String], Nil, r4)._1, Vector.empty)
    assertEquals(drive(SortMerge.left[Int, String, String], l3, Nil)._1, l3.map((k, a) => (k, (a, None))).toVector)
    assertEquals(drive(SortMerge.full[Int, String, String], Nil, r4)._1, r4.map((k, b) => (k, (None, Some(b)))).toVector)
    assertEquals(drive(SortMerge.full[Int, String, String], Nil, Nil)._1, Vector.empty)
    // an inner join whose left side ended asks for nothing more of the right
    val m = SortMerge.inner[Int, String, String]
    assertEquals(m.step(_ => ()), Need.Left)
    m.leftEnd()
    assertEquals(m.step(_ => ()), Need.Done)
  }

  test("a key out of order on either side fails, naming the side and both keys") {
    val eL = intercept[IllegalArgumentException](drive(SortMerge.inner[Int, String, String], List((2, "a"), (1, "b")), r4))
    assert(eL.getMessage.contains("left side") && eL.getMessage.contains("key 1 after 2"), eL.getMessage)
    val eR = intercept[IllegalArgumentException](drive(SortMerge.inner[Int, String, String], l3, List((3, "x"), (2, "y"))))
    assert(eR.getMessage.contains("right side") && eR.getMessage.contains("key 2 after 3"), eR.getMessage)
    // equal keys are a run, not a violation; a reverse Ordering joins descending sides
    assertEquals(drive(SortMerge.inner[Int, String, String](using Ordering.Int.reverse), l3.reverse, r4.reverse)._1,
      Vector((4, ("d", "w")), (2, ("b", "x"))))
  }

  // ------------------------------------------------------------ Chunks

  private def chunked[A](xs: List[A], size: Int): Chunks[A] = Chunks.fromIterator(xs.iterator, size)

  test("Chunks.joinSorted agrees with the hash join across chunk boundaries of different sizes") {
    for seed <- 1 to 12; (sl, sr) <- List((1, 1), (3, 5), (5, 3), (64, 2)) do
      val rnd = Random(seed)
      val l = sortedRows(rnd, rnd.nextInt(80), 1 + rnd.nextInt(10), "l")
      val r = sortedRows(rnd, rnd.nextInt(80), 1 + rnd.nextInt(10), "r")
      note(s"seed $seed sizes $sl/$sr: ${l.size} x ${r.size}")
      assertEquals(Chunks.joinSorted(chunked(l, sl), chunked(r, sr)).elements.toVector, hashJoin(l, r), s"seed $seed sizes $sl/$sr")
  }

  test("Chunks: left and full agree with the machine over lists") {
    assertEquals(Chunks.leftJoinSorted(chunked(l3, 2), chunked(r4, 3)).elements.toVector,
      drive(SortMerge.left[Int, String, String], l3, r4)._1)
    assertEquals(Chunks.fullJoinSorted(chunked(l3, 2), chunked(r4, 3)).elements.toVector,
      drive(SortMerge.full[Int, String, String], l3, r4)._1)
  }

  test("one output chunk per left chunk, and the join is lazy on endless sorted sides") {
    val l = List.tabulate(10)(i => (i, s"l$i"))
    val r = List.tabulate(10)(i => (i, s"r$i"))
    assertEquals(Chunks.joinSorted(chunked(l, 3), chunked(r, 4)).toLazyList.map(_.length).toList, List(3, 3, 3, 1))
    val nats = Chunks.map(Chunks.nats[Long](8))(k => (k, k))
    val evens = Chunks.map(Chunks.nats[Long](5))(k => (k * 2, k))
    assertEquals(Chunks.joinSorted(nats, evens).elements.take(3).toList, List((0L, (0L, 0L)), (2L, (2L, 1L)), (4L, (4L, 2L))))
  }

  test("Chunks: a key out of order fails at its row, the chunks before it told") {
    val l = chunked(List((0, "a"), (1, "b"), (3, "c"), (2, "d")), 1)
    val it = Chunks.joinSorted(l, chunked(List((0, "x"), (1, "y"), (2, "z"), (3, "w")), 64)).elements
    assertEquals(it.next(), (0, ("a", "x")))
    assertEquals(it.next(), (1, ("b", "y")))
    // the row at 3 is decided before the row at 2 arrives: it is told
    assertEquals(it.next(), (3, ("c", "w")))
    val e = intercept[IllegalArgumentException](it.next())
    assert(e.getMessage.contains("left side"), e.getMessage)
  }
}
