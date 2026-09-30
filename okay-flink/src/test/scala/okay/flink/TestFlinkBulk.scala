package okay.flink

import okay.*
import okay.Chunks.elements
import okay.Tables.{collect, join, select}
import scala.util.Random

/**
 * `FlinkBulk` (bulk-flink; specs/streams-seam.md, lane 3): Flink as a
 * `Bulk`, bounded. The law is AGREEMENT with the local instance. `Live`:
 * a local MiniCluster, as the Wrocław Flink lane is.
 */
class TestFlinkBulk extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitTimeout = scala.concurrent.duration.Duration(5, "min")

  lazy val flink: Bulk[FlinkBulk.Rows] = FlinkBulk.local(2)
  val local: Bulk[Chunks] = Bulk.local(_ => Iterator.empty)

  test("of, map, flatMap, filter, join, aggregate and toChunks answer what the local instance answers") {
    for seed <- 1 to 3 do
      val rnd = Random(seed)
      val xs = List.fill(200)(rnd.nextInt(40))
      val ys = List.fill(30)((rnd.nextInt(40), rnd.alphanumeric.take(3).mkString))
      def pipeline[D[_]](B: Bulk[D]): (Vector[(Int, (Int, String))], Long, Vector[Int]) =
        val l = B.filter(B.flatMap(B.map(B.of(xs))(_ + 1))(x => List(x, x * 2)))(_ % 3 != 0)
        val joined = B.join(B.map(l)(x => (x, x)), B.of(ys))
        val sum = B.aggregate(l)(Aggregator.sum[Long].contramap[Int](_.toLong))
        (B.toChunks(joined).elements.toVector.sorted, sum, B.toChunks(l).elements.toVector.sorted)
      assertEquals(pipeline(flink), pipeline(local), s"seed $seed")
  }

  test("a Tables program runs on Flink unchanged") {
    val p = Tables.of(Vector.range(0, 500)).select(i => (i % 7, i)).join(Tables.of(Vector.tabulate(7)(k => (k, s"k$k"))))
      .collect.map(_.elements.toVector.sorted)
    assertEquals(Tables.run(flink)(p), Tables.run(local)(p))
  }

  test("an empty collection aggregates to the aggregator's empty answer") {
    assertEquals(flink.aggregate(flink.of(List.empty[Int]))(Aggregator.count[Int]), 0L)
  }
