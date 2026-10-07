package okay


import scala.util.Random
import Chunks.elements

/** `ExternalSort` (chunks-external-sort) over runs kept in memory, so the
 * run count is readable and the law holds on every platform */
class TestExternalSort extends munit.FunSuite {

  private def input(seed: Int, n: Int): Vector[(Int, String)] =
    val rnd = Random(seed)
    Vector.tabulate(n)(i => (rnd.nextInt(50), s"v$i"))

  test("the sorted output is the stable in-memory sort, at budgets 1, 7, 64 and above the input") {
    for seed <- 1 to 5; budget <- List(1, 7, 64, 100000) do
      val xs = input(seed, 500 + seed * 37)
      given spill: Spill.Memory = Spill.memory
      val got = Chunks.sortBy(Chunks.fromIterator(xs.iterator, 13), budget)(_._1).elements.toVector
      assertEquals(got, xs.sortBy(_._1), s"seed $seed budget $budget")
      val runs = if xs.length <= budget then 0 else (xs.length + budget - 1) / budget
      assertEquals(spill.opened, runs, s"seed $seed budget $budget: runs")
      assertEquals(spill.live, 0, "a run outlived the output read to its end")
  }

  test("nothing is read before the first pull; a descending order; an empty input") {
    var reads = 0
    given Spill = Spill.memory
    val lazily = Chunks.sortBy(Chunks.fromIterator(Iterator.tabulate(100)(i => { reads += 1; i.toLong })), 10)(identity)
    assertEquals(reads, 0)
    assertEquals(lazily.elements.take(1).toList, List(0L))
    val down = Chunks.sortBy(Chunks.fromIterator(Iterator.range(0, 50).map(_.toLong)), 8)(identity)(using Ordering.Long.reverse)
    assertEquals(down.elements.toVector, Vector.range(0L, 50L).reverse)
    assertEquals(Chunks.sortBy(Chunks.fromIterator(Iterator.empty[Long]), 4)(identity).elements.toVector, Vector.empty)
  }
}
