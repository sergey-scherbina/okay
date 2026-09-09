package okay

import okay.Chunks.elements
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/** The local `Bulk` and the CSV it reads (specs/bulk.md). */
class TestBulk extends munit.FunSuite {

  test("Csv.fields: quotes, doubled quotes, commas inside quotes") {
    assertEquals(Csv.fields("a,\"b,c\",\"say \"\"hi\"\"\",,d"), Vector("a", "b,c", "say \"hi\"", "", "d"))
    assertEquals(Csv.fields(""), Vector(""))
  }

  test("Csv.rows: the header names the columns and a BOM is stripped") {
    val rows = Csv.rows(Iterator("﻿id,name", "1,\"Krasińskiego\"", "2,KŁOKOCZYCE")).toList
    assertEquals(rows, List(Map("id" -> "1", "name" -> "Krasińskiego"), Map("id" -> "2", "name" -> "KŁOKOCZYCE")))
  }

  test("a local source is replayable: a file is re-read on every run") {
    val f = Files.createTempFile("bulk", ".csv")
    Files.writeString(f, "k,v\n1,10\n2,20\n1,5\n")
    var reads = 0
    val B = Bulk.local(p => { reads += 1; Files.lines(java.nio.file.Path.of(p)).iterator().asScala })
    // B.map, not d.map: on a CONCRETE Chunks the collection view loses to
    // the program's own monadic map — the view is for code generic in D
    val d = B.map(B.csv(f.toString))(r => r("k").toInt -> r("v").toLong)
    assertEquals(B.aggregate(d)(Aggregator.count[(Int, Long)]), 3L)
    assertEquals(B.aggregate(d)(Aggregator.sum[Long].contramap[(Int, Long)](_._2)), 35L)
    assertEquals(reads, 2)
    // and cached, it is read once more and then held
    val c = B.cache(d)
    assertEquals(B.aggregate(c)(Aggregator.count[(Int, Long)]), 3L)
    assertEquals(B.aggregate(c)(Aggregator.count[(Int, Long)]), 3L)
    assertEquals(reads, 3)
    Files.delete(f)
  }

  test("join is the equi-join, right side hashed, left side streamed") {
    val B = localBulk
    val l = B.of(List(1 -> "a", 2 -> "b", 1 -> "c", 3 -> "d"))
    val r = B.of(List(1 -> 10, 1 -> 11, 2 -> 20))
    val all = Aggregator[(Int, (String, Int)), Vector[(Int, (String, Int))], Vector[(Int, (String, Int))]](Vector.empty)(_ :+ _)(_ ++ _)(identity)
    val joined = B.aggregate(B.join(l, r))(all)
    assertEquals(joined.sorted, Vector(1 -> ("a", 10), 1 -> ("a", 11), 1 -> ("c", 10), 1 -> ("c", 11), 2 -> ("b", 20)))
  }
}

/** The effect layer over the seam: a program of tables, run and traced (specs/bulk.md). */
class TestTables extends munit.FunSuite {
  import okay.Tables.of
  import okay.Sort.sortBy
  import okay.RowLift.plus

  final case class Sale(shop: Int, amount: Long)

  /** the plan: a VALUE, no platform in its type */
  def revenue(sales: Iterable[Sale], cities: Iterable[(Int, String)]): Map[String, Long] ! Tables =
    of(sales).select(s => s.shop -> s.amount)
      .join(of(cities))
      .select { case (_, (amount, city)) => (city, amount) }
      .aggregate(Aggregator.groupBy((kv: (String, Long)) => kv._1)(Aggregator.sum[Long].contramap(_._2)))

  val sales = (1 to 2000).map(i => Sale(i % 7, (i % 13).toLong))
  val cities = (0 until 7).map(i => i -> (if i % 2 == 0 then "Wrocław" else "Kraków"))
  val city = cities.toMap

  test("the same plan on the local platform equals the direct computation") {
    val got = Tables.run(localBulk)(revenue(sales, cities))
    val want = sales.groupMapReduce(s => city(s.shop))(_.amount)(_ + _)
    assertEquals(got, want)
  }

  test("a plan is data: tracing prints it before anything runs") {
    val traced = !.tracing(revenue(sales, cities).plus[okay.Pure])([X] => (e: Tables[X]) => e.productPrefix)
    val handled = State.handle(Tables.Heap.empty[Chunks])(Tables.via(localBulk)(traced))
    val (plan, (_, got)) = !.run(Writer.run(handled))
    assertEquals(plan, Seq("Of", "Select", "Of", "Join", "Select", "Aggregate"))
    assertEquals(got, sales.groupMapReduce(s => city(s.shop))(_.amount)(_ + _))
  }

  test("Sort is an operation Bulk does not have, answered through the primitives") {
    val prog: Vector[(Int, Long)] ! (Tables + Sort) =
      of(sales).select(s => s.shop -> s.amount).plus[Sort]
        .aggregate(Aggregator.groupBy((kv: (Int, Long)) => kv._1)(Aggregator.sum[Long].contramap(_._2)))
        .flatMap(byShop => of(byShop.toVector).plus[Sort].sortBy(-_._2).collect.map(_.elements.toVector))
    val got = Tables.run(localBulk)(Sort.viaTables(prog))
    assertEquals(got.map(_._1), sales.groupMapReduce(_.shop)(_.amount)(_ + _).toVector.sortBy(-_._2).map(_._1))
  }
}
