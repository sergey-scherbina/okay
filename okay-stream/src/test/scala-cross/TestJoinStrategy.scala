package okay

import scala.util.Random
import Chunks.elements
import Tables.{Plan, Table, collect, join, sortByKey, assumeSortedByKey, select, where}

/** join-strategy-auto: the plan picks how a join runs, the program can
 * say, and every road answers the same pairs */
class TestJoinStrategy extends munit.FunSuite {

  private val B: Bulk[Chunks] = Bulk.local(_ => Iterator.empty)

  /** run `p`, keeping the plans it forced */
  private def traced[A](p: A ! Tables): (A, List[String]) =
    val plans = scala.collection.mutable.ListBuffer.empty[String]
    val a = State.run(Tables.Heap.empty[Chunks])(Tables.via[A, Chunks, Pure](B, p => plans += Plan.show(p))(p))._2
    (a, plans.toList)

  private def rows(seed: Int, n: Int, tag: String): Vector[(Int, String)] =
    val rnd = Random(seed)
    Vector.fill(n)((rnd.nextInt(20), s"$tag${rnd.nextInt(1000)}"))

  private val l = rows(1, 200, "l")
  private val r = rows(2, 80, "r")
  private val expected = (for (k, a) <- l; (k2, b) <- r if k == k2 yield (k, (a, b))).sorted

  test("two sorted sides merge; unsorted sides hash; the answer is the same") {
    val (merged, p1) = traced(Tables.of(l).sortByKey.join(Tables.of(r).sortByKey).collect.map(_.elements.toVector.sorted))
    assertEquals(merged, expected)
    assert(p1.exists(_.contains("Join(sort-merge)")), p1.mkString("\n"))
    val (hashed, p2) = traced(Tables.of(l).join(Tables.of(r)).collect.map(_.elements.toVector.sorted))
    assertEquals(hashed, expected)
    assert(p2.exists(_.contains("Join(hash)")), p2.mkString("\n"))
  }

  test("the fact survives a Where and dies at a Select; one side sorted is not enough; two orderings are not one") {
    val (_, kept) = traced(Tables.of(l).sortByKey.where(_._1 > 3).join(Tables.of(r).sortByKey).collect)
    assert(kept.exists(_.contains("Join(sort-merge)")), kept.mkString("\n"))
    val (_, lost) = traced(Tables.of(l).sortByKey.select(identity).join(Tables.of(r).sortByKey).collect)
    assert(lost.exists(_.contains("Join(hash)")), lost.mkString("\n"))
    val (_, half) = traced(Tables.of(l).sortByKey.join(Tables.of(r)).collect)
    assert(half.exists(_.contains("Join(hash)")), half.mkString("\n"))
    val down: Table[(Int, String)] ! Tables = { given Ordering[Int] = Ordering.Int.reverse; Tables.of(l).sortByKey }
    val (_, two) = traced(down.join(Tables.of(r).sortByKey).collect)
    assert(two.exists(_.contains("Join(hash)")), two.mkString("\n"))
  }

  test("the caller's word is a fact too, and checked: an unsorted side said sorted fails by name") {
    val (ok, p) = traced(Tables.of(l.sortBy(_._1)).assumeSortedByKey.join(Tables.of(r.sortBy(_._1)).assumeSortedByKey)
      .collect.map(_.elements.toVector.sorted))
    assertEquals(ok, expected)
    assert(p.exists(_.contains("Join(sort-merge)")), p.mkString("\n"))
    val e = intercept[IllegalArgumentException](
      traced(Tables.of(Vector((2, "a"), (1, "b"))).assumeSortedByKey.join(Tables.of(Vector((1, "x"), (3, "y"))).assumeSortedByKey).collect.map(_.elements.toVector)))
    // (the right side outlives the bad row: an inner merge ends at either side's end,
    //  so a row out of order after that end is never read)
    assert(e.getMessage.contains("not sorted"), e.getMessage)
  }

  test("the program can say: Hash on sorted sides hashes, SortMerge on sorted sides merges") {
    val (h, ph) = traced(Tables.of(l).sortByKey.join(Tables.of(r).sortByKey, JoinStrategy.Hash()).collect.map(_.elements.toVector.sorted))
    assertEquals(h, expected)
    assert(ph.exists(_.contains("Join(hash)")), ph.mkString("\n"))
    val (m, pm) = traced(Tables.of(l).sortByKey.join(Tables.of(r).sortByKey, JoinStrategy.sortMerge[Int]).collect.map(_.elements.toVector.sorted))
    assertEquals(m, expected)
    assert(pm.exists(_.contains("Join(sort-merge)")), pm.mkString("\n"))
  }
}
