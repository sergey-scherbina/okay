package okay


import okay.freer.*


import okay.std.*
import scala.util.Random
import Chunks.elements

/**
 * `BulkParallel` (bulk-local-parallel): the parallel local instance
 * answers what `Bulk.local` answers — the splits in split order, the
 * aggregate merged in input order (a `Sequential` included), the join's
 * rows in the left side's order.
 */
class TestBulkParallel extends munit.FunSuite with okay.testkit.Munit.Diagnosed {

  private val local = Bulk.local(_ => Iterator.empty)

  private def splitsFormat(n: Int, per: Int): Bulk.Format[Int] = new Bulk.Format[Int]:
    def name = "ints"
    def splits(path: String): Vector[Int] = Vector.range(0, n)
    def read(path: String, split: Int): Iterator[Int] =
      Thread.sleep(1)                                   // a split is work: let the fibres overlap
      Iterator.range(split * per, split * per + per)

  test("read: every split once, in split order, at 1, 4 and 16 fibres") {
    for par <- List(1, 4, 16) do
      val B = BulkParallel(par)
      assertEquals(B.read("f", splitsFormat(37, 10)).elements.toVector, Vector.range(0, 370), s"parallelism $par")
      assertEquals(B.read("f", splitsFormat(0, 10)).elements.toVector, Vector.empty, s"no splits, parallelism $par")
  }

  test("aggregate: the local answer, including an order-dependent merge") {
    val concat = new Aggregator[Int, Vector[Int], Vector[Int]]:
      def init = Vector.empty
      def add(v: Vector[Int], x: Int) = v :+ x
      def merge(a: Vector[Int], b: Vector[Int]) = a ++ b              // associative, NOT commutative
      def present(v: Vector[Int]) = v
    for seed <- 1 to 5; par <- List(1, 3, 8) do
      val rnd = Random(seed)
      val xs = Vector.fill(rnd.nextInt(20000))(rnd.nextInt(1000))
      val B = BulkParallel(par)
      note(s"seed $seed parallelism $par: ${xs.size}")
      assertEquals(B.aggregate(B.of(xs))(Aggregator.sum[Long].contramap[Int](_.toLong)), xs.map(_.toLong).sum)
      assertEquals(B.aggregate(B.of(xs))(concat), xs, "the merge ran out of input order")
  }

  test("join: the local join's rows, in the left side's order") {
    for seed <- 1 to 5; par <- List(1, 4) do
      val rnd = Random(seed)
      val l = Vector.fill(rnd.nextInt(3000))((rnd.nextInt(50), rnd.nextInt()))
      val r = Vector.fill(rnd.nextInt(300))((rnd.nextInt(50), rnd.alphanumeric.take(2).mkString))
      val B = BulkParallel(par)
      val got = B.join(B.of(l), B.of(r)).elements.toVector
      val expected = local.join(local.of(l), local.of(r)).elements.toVector
      assertEquals(got.map(_._1), expected.map(_._1), s"seed $seed parallelism $par: the left order")
      assertEquals(got.sorted, expected.sorted, s"seed $seed parallelism $par")
  }

  test("a Tables program runs on it unchanged, and a re-read source is re-read") {
    import Tables.{collect, join, select}
    val B = BulkParallel(4)
    val p = Tables.of(Vector.range(0, 1000)).select(i => (i % 7, i)).join(Tables.of(Vector.tabulate(7)(k => (k, s"k$k"))))
      .collect.map(_.elements.toVector.sorted)
    assertEquals(Tables.run(B)(p), Tables.run(local)(p))
    var reads = 0
    val counting = new Bulk.Format[Int]:
      def name = "counting"
      def splits(path: String) = Vector(0, 1)
      def read(path: String, split: Int) = { synchronized(reads += 1); Iterator(split) }
    val d = B.read("f", counting)
    assertEquals(d.elements.toVector, Vector(0, 1))
    assertEquals(d.elements.toVector, Vector(0, 1))
    assertEquals(reads, 4, "each consumption reads the file")
  }
}
