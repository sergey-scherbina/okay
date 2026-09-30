package okay2.stream

import scala.util.Random
import okay2.stream.Chunks.ChunksOps

/**
 * The sort-merge join by key (specs/stream-join.md), the Scala 2 twin
 * of the core's TestSortMerge: the machine over two lists, then the
 * chunk driver against a hash join written here.
 */
class TestSortMerge extends munit.FunSuite {
  import SortMerge.Need

  private def drive[K, A, B, O](m: SortMerge[K, A, B, O], l: List[(K, A)], r: List[(K, B)]): (Vector[O], Int) = {
    val out = Vector.newBuilder[O]
    var ls = l; var rs = r; var maxHeld = 0
    var done = false
    while (!done) {
      m.step(o => { out += o; () }) match {
        case Need.Done => done = true
        case Need.Left => ls match {
          case (k, a) :: t => m.left(k, a); ls = t
          case Nil => m.leftEnd()
        }
        case Need.Right => rs match {
          case (k, b) :: t => m.right(k, b); rs = t
          case Nil => m.rightEnd()
        }
      }
      maxHeld = maxHeld max m.held
    }
    (out.result(), maxHeld)
  }

  /** the reference: the right side hashed, the left side streamed (Bulk.local's join) */
  private def hashJoin[K, A, B](l: List[(K, A)], r: List[(K, B)]): Vector[(K, (A, B))] = {
    val right = r.groupBy(_._1).map { case (k, xs) => k -> xs.map(_._2) }
    l.flatMap { case (k, a) => right.getOrElse(k, Nil).map(b => (k, (a, b))) }.toVector
  }

  private def sortedRows(rnd: Random, n: Int, keys: Int, tag: String): List[(Int, String)] =
    List.fill(n)(rnd.nextInt(keys)).sorted.zipWithIndex.map { case (k, i) => (k, s"$tag$i") }

  private val l3 = List((1, "a"), (2, "b"), (4, "d"))
  private val r4 = List((2, "x"), (3, "y"), (3, "z"), (4, "w"))

  test("the inner join is the hash join's answer, in its order, on random sorted runs") {
    for (seed <- 1 to 30) {
      val rnd = new Random(seed)
      val l = sortedRows(rnd, rnd.nextInt(60), 1 + rnd.nextInt(12), "l")
      val r = sortedRows(rnd, rnd.nextInt(60), 1 + rnd.nextInt(12), "r")
      assertEquals(drive(SortMerge.inner[Int, String, String], l, r)._1, hashJoin(l, r), s"seed $seed")
    }
  }

  test("a run of m left and n right rows produces m x n pairs, left-major, holding n rows") {
    val l = List((5, "a"), (5, "b"), (5, "c"))
    val r = List((5, "x"), (5, "y"), (5, "z"), (5, "w"))
    val (out, held) = drive(SortMerge.inner[Int, String, String], l, r)
    assertEquals(out, (for { a <- l; b <- r } yield (5, (a._2, b._2))).toVector)
    assertEquals(held, 4)
    val (out2, held2) = drive(SortMerge.inner[Int, Int, String], List.tabulate(10000)(i => (7, i)), List((7, "x"), (7, "y")))
    assertEquals(out2.size, 20000)
    assertEquals(held2, 2)
  }

  test("the unmatched side per variant, and an empty side") {
    assertEquals(drive(SortMerge.inner[Int, String, String], l3, r4)._1, Vector((2, ("b", "x")), (4, ("d", "w"))))
    assertEquals(drive(SortMerge.left[Int, String, String], l3, r4)._1,
      Vector((1, ("a", None)), (2, ("b", Some("x"))), (4, ("d", Some("w")))))
    assertEquals(drive(SortMerge.full[Int, String, String], l3, r4)._1,
      Vector((1, (Some("a"), None)), (2, (Some("b"), Some("x"))),
             (3, (None, Some("y"))), (3, (None, Some("z"))), (4, (Some("d"), Some("w")))))
    assertEquals(drive(SortMerge.full[Int, String, String], l3, r4 :+ (7, "q"))._1.last, (7, (None, Some("q"))))
    assertEquals(drive(SortMerge.inner[Int, String, String], l3, Nil)._1, Vector.empty[(Int, (String, String))])
    assertEquals(drive(SortMerge.left[Int, String, String], l3, Nil)._1, l3.map { case (k, a) => (k, (a, None)) }.toVector)
    assertEquals(drive(SortMerge.full[Int, String, String], Nil, r4)._1, r4.map { case (k, b) => (k, (None, Some(b))) }.toVector)
    val m = SortMerge.inner[Int, String, String]
    assertEquals(m.step(_ => ()), Need.Left: Need)
    m.leftEnd()
    assertEquals(m.step(_ => ()), Need.Done: Need)
  }

  test("a key out of order on either side fails, naming the side and both keys") {
    val eL = intercept[IllegalArgumentException](drive(SortMerge.inner[Int, String, String], List((2, "a"), (1, "b")), r4))
    assert(eL.getMessage.contains("left side") && eL.getMessage.contains("key 1 after 2"), eL.getMessage)
    val eR = intercept[IllegalArgumentException](drive(SortMerge.inner[Int, String, String], l3, List((3, "x"), (2, "y"))))
    assert(eR.getMessage.contains("right side") && eR.getMessage.contains("key 2 after 3"), eR.getMessage)
    assertEquals(drive(SortMerge.inner[Int, String, String](Ordering.Int.reverse), l3.reverse, r4.reverse)._1,
      Vector((4, ("d", "w")), (2, ("b", "x"))))
  }

  private def chunked[A](xs: List[A], size: Int): Chunks[A] = Chunks.fromIterator(xs.iterator, size)

  test("Chunks.joinSorted agrees with the hash join across chunk boundaries; left and full; one output chunk per left chunk; lazy") {
    for { seed <- 1 to 12; (sl, sr) <- List((1, 1), (3, 5), (5, 3), (64, 2)) } {
      val rnd = new Random(seed)
      val l = sortedRows(rnd, rnd.nextInt(80), 1 + rnd.nextInt(10), "l")
      val r = sortedRows(rnd, rnd.nextInt(80), 1 + rnd.nextInt(10), "r")
      assertEquals(Chunks.joinSorted(chunked(l, sl), chunked(r, sr)).elements.toVector, hashJoin(l, r), s"seed $seed sizes $sl/$sr")
    }
    assertEquals(Chunks.leftJoinSorted(chunked(l3, 2), chunked(r4, 3)).elements.toVector, drive(SortMerge.left[Int, String, String], l3, r4)._1)
    assertEquals(Chunks.fullJoinSorted(chunked(l3, 2), chunked(r4, 3)).elements.toVector, drive(SortMerge.full[Int, String, String], l3, r4)._1)
    val ten = List.tabulate(10)(i => (i, s"$i"))
    assertEquals(Chunks.joinSorted(chunked(ten, 3), chunked(ten, 4)).toLazyList.map(_.length).toList, List(3, 3, 3, 1))
    val nats = Chunks.map(Chunks.range(0, Long.MaxValue, 8))(k => (k, k))
    val evens = Chunks.map(Chunks.range(0, Long.MaxValue, 5))(k => (k * 2, k))
    assertEquals(Chunks.joinSorted(nats, evens).elements.take(3).toList, List((0L, (0L, 0L)), (2L, (2L, 1L)), (4L, (4L, 2L))))
  }
}
