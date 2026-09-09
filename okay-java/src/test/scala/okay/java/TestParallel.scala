package okay.java

import okay.{Aggregator, Bulk, Chunks}
import okay.Bulk.*
import java.util.List as JList

/**
 * The `Bulk` seam on the JDK's cores against the local instance: one
 * program, two platforms, equal answers (specs/bulk.md).
 */
class TestParallel extends munit.FunSuite {

  final case class Sale(shop: Int, amount: Long)

  /** a small ETL, written once against the seam */
  def revenueByCity[D[_]](sales: Iterable[Sale], cities: Iterable[(Int, String)])(using B: Bulk[D]): Map[String, Long] =
    B.of(sales).map(s => s.shop -> s.amount)
      .join(B.of(cities))
      .map { case (_, (amount, city)) => (city, amount) }
      .aggregate(Aggregator.groupBy((kv: (String, Long)) => kv._1)(Aggregator.sum[Long].contramap(_._2)))

  val sales = (1 to 20000).map(i => Sale(i % 7, (i % 13).toLong))
  val cities = (0 until 7).map(i => i -> (if i % 2 == 0 then "Wrocław" else "Kraków"))

  test("parallel streams and one JVM agree on a join and a grouped sum") {
    val onCores = revenueByCity[JList](sales, cities)(using Parallel.bulk)
    val local = revenueByCity[Chunks](sales, cities)(using okay.localBulk)
    assertEquals(onCores, local)
    assertEquals(onCores.values.sum, sales.map(_.amount).sum)
  }

  test("a list is consumed as often as the program likes") {
    import Parallel.given
    val d = Parallel.bulk.of(1 to 100).map(_ * 2)
    assertEquals(d.aggregate(Aggregator.sum[Int]), 10100)
    assertEquals(d.aggregate(Aggregator.count[Int]), 100L)
  }
}
