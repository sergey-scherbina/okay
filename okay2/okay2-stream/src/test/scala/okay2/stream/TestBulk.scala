package okay2.stream

import java.nio.file.Files
import scala.jdk.CollectionConverters._
import okay2._
import okay2.stream.Chunks.ChunksOps
import TablesFixtures._

/** The local `Bulk`, the CSV it reads, and a program of tables over it
 * — the Scala 3 core's okay-stream TestBulk and TestTables */
class TestBulk extends munit.FunSuite {

  test("Csv.fields: quotes, doubled quotes, commas inside quotes") {
    assertEquals(Csv.fields("a,\"b,c\",\"say \"\"hi\"\"\",,d"), Vector("a", "b,c", "say \"hi\"", "", "d"))
    assertEquals(Csv.fields(""), Vector(""))
  }

  test("Csv.line is the inverse of Csv.fields, quoting only what needs it") {
    val row = Vector("a", "b,c", "say \"hi\"", "", "plain")
    assertEquals(Csv.line(row), "a,\"b,c\",\"say \"\"hi\"\"\",,plain")
    assertEquals(Csv.fields(Csv.line(row)), row)
  }

  test("Csv.rows: the header names the columns and a BOM is stripped") {
    val rows = Csv.rows(Iterator("﻿id,name", "1,\"Krasińskiego\"", "2,KŁOKOCZYCE")).toList
    assertEquals(rows, List(Map("id" -> "1", "name" -> "Krasińskiego"), Map("id" -> "2", "name" -> "KŁOKOCZYCE")))
  }

  test("a local source is replayable: a file is re-read on every run, and cache holds it") {
    val f = Files.createTempFile("bulk", ".csv")
    Files.writeString(f, "k,v\n1,10\n2,20\n1,5\n")
    var reads = 0
    val B = Bulk.local(p => { reads += 1; Files.lines(java.nio.file.Path.of(p)).iterator().asScala })
    val d = B.map(B.csv(f.toString))(r => r("k").toInt -> r("v").toLong)
    assertEquals(B.aggregate(d)(Aggregator.count[(Int, Long)]), 3L)
    assertEquals(B.aggregate(d)(Aggregator.sum[Long].contramap[(Int, Long)](_._2)), 35L)
    assertEquals(reads, 2)
    val c = B.cache(d)
    assertEquals(B.aggregate(c)(Aggregator.count[(Int, Long)]), 3L)
    assertEquals(B.aggregate(c)(Aggregator.count[(Int, Long)]), 3L)
    assertEquals(reads, 3)
    Files.delete(f)
  }

  test("join is the equi-join, right side hashed, left side streamed") {
    val B = localBulk
    val joined = B.toChunks(B.join(B.of(List(1 -> "a", 2 -> "b", 1 -> "c", 3 -> "d")), B.of(List(1 -> 10, 1 -> 11, 2 -> 20)))).elements.toVector
    assertEquals(joined.sorted, Vector(1 -> (("a", 10)), 1 -> (("a", 11)), 1 -> (("c", 10)), 1 -> (("c", 11)), 2 -> (("b", 20))))
  }

  // ---- the effect over the seam

  /** the plan: a VALUE, no platform in its type */
  def revenue(sales: Iterable[Sale], cities: Iterable[(Int, String)]): Map[String, Long] ! Tables =
    Tables.of(sales).select(s => s.shop -> s.amount)
      .join(Tables.of(cities))
      .select { case (_, (amount, city)) => (city, amount) }
      .aggregate(Aggregator.groupBy((kv: (String, Long)) => kv._1)(Aggregator.sum[Long].contramap[(String, Long)](_._2)))

  val sales = (1 to 2000).map(i => Sale(i % 7, (i % 13).toLong))
  val cities = (0 until 7).map(i => i -> (if (i % 2 == 0) "Wrocław" else "Kraków"))
  val city = cities.toMap

  test("the same plan on the local platform equals the direct computation") {
    assertEquals(Tables.run(localBulk)(revenue(sales, cities)), sales.groupMapReduce(s => city(s.shop))(_.amount)(_ + _))
  }

  test("a plan is data: the handler sees each plan as it forces it") {
    val plans = scala.collection.mutable.ListBuffer.empty[String]
    val got = State.run(Tables.Heap.empty[Chunks])(Tables.via[Map[String, Long], Chunks, Pure](localBulk, p => { plans += Tables.Plan.show(p); () })(revenue(sales, cities)))._2
    assertEquals(got, sales.groupMapReduce(s => city(s.shop))(_.amount)(_ + _))
    assertEquals(plans.size, 1, "one action, one forced plan")
    assert(plans.head.contains("Join") && plans.head.startsWith("Select"), plans.head)
  }

  test("Sort is an operation Bulk does not have, answered through the primitives") {
    import Sort.SortProgramOps
    val prog: Free[Tables with Sort, Vector[(Int, Long)]] =
      (Tables.of(sales).select(s => s.shop -> s.amount): Free[Tables with Sort, Tables.Table[(Int, Long)]])
        .aggregate(Aggregator.groupBy((kv: (Int, Long)) => kv._1)(Aggregator.sum[Long].contramap[(Int, Long)](_._2)))
        .flatMap(byShop => (Tables.of(byShop.toVector): Free[Tables with Sort, Tables.Table[(Int, Long)]]).sortBy(-_._2).collect.map(_.elements.toVector))
    val got = Tables.run(localBulk)(Sort.viaTables[Vector[(Int, Long)], Tables](prog))
    assertEquals(got.map(_._1), sales.groupMapReduce(_.shop)(_.amount)(_ + _).toVector.sortBy(-_._2).map(_._1))
  }
}
