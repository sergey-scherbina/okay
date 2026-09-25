package okay2.spark

import okay2._
import okay2.stream.{Chunks, Sort, Tables}
import okay2.stream.Chunks.ChunksOps
import okay2.stream.Sort._
import okay2.stream.Tables.{Ctx, Heap, Table}
import org.apache.spark.sql.SparkSession

/**
 * `SparkBulk` (the Scala 3 core's `okay.spark.SparkBulk`, ported): a
 * program written against `Bulk[D]` names no platform — the same
 * answer over local `Chunks` and over Spark's own RDDs. No dedicated
 * test exists for this on the Scala 3 side either (its own coverage is
 * the Wrocław GTFS integration suites); this one is new, aimed
 * directly at the seam.
 */
class TestSparkBulk extends munit.FunSuite {

  // see TestSparkInterop's own comment: Spark's local[2] bring-up can
  // exceed munit's 30s default on a shared box.
  override def munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(3, "min")

  lazy val spark = SparkSession.builder()
    .master("local[2]").appName("okay2-spark-bulk-test")
    .config("spark.ui.enabled", "false")
    .getOrCreate()

  override def afterAll(): Unit = spark.stop()

  lazy val B: SparkBulk.SparkBulk = SparkBulk(spark)

  test("of/map/filter: the same answer as the local Bulk") {
    val xs = 1 to 100
    val rows = B.of(xs)
    val mapped = B.map(rows)(_ * 2)
    val filtered = B.filter(mapped)(_ % 4 == 0)
    val got = B.toChunks(filtered).elements.toVector.sorted
    val want = xs.map(_ * 2).filter(_ % 4 == 0).toVector.sorted
    assertEquals(got, want)
  }

  test("join: an equi-join over two Rows, same answer as a local Map join") {
    val l = B.of((1 to 20).map(i => (i % 5, i)))
    val r = B.of((0 to 4).map(k => (k, s"k$k")))
    val got = B.toChunks(B.join(l, r)).elements.toSet
    val want = (1 to 20).map(i => (i % 5, i)).map { case (k, i) => (k, (i, s"k$k")) }.toSet
    assertEquals(got, want)
  }

  test("aggregate: the same Aggregator, local Chunks and Spark, equal") {
    val xs = (1 to 1000).map(_.toLong)
    val agg = Aggregator.sum[Long]
    val local = agg.present(Chunks.fold(Chunks.fromIterator(xs.iterator))(agg.fold))
    val onSpark = B.aggregate(B.of(xs))(agg)
    assertEquals(onSpark, local)
  }

  test("cache: the cached Rows still answer the same aggregate") {
    val cached = B.cache(B.of(1 to 500))
    assertEquals(B.aggregate(cached)(Aggregator.count[Int]), 500L)
    assertEquals(B.aggregate(cached)(Aggregator.count[Int]), 500L)
  }

  test("csv: a header-first file, read the platform's way, column-pruned") {
    val f = java.nio.file.Files.createTempFile("okay2-spark-bulk", ".csv")
    try {
      java.nio.file.Files.writeString(f, "a,b,c\n1,2,3\n4,5,6\n")
      val all = B.toChunks(B.csv(f.toString)).elements.toSet
      assertEquals(all, Set(Map("a" -> "1", "b" -> "2", "c" -> "3"), Map("a" -> "4", "b" -> "5", "c" -> "6")))
      val pruned = B.toChunks(B.csv(f.toString, Some(Set("a", "c")))).elements.toSet
      assertEquals(pruned, Set(Map("a" -> "1", "c" -> "3"), Map("a" -> "4", "c" -> "6")))
    } finally java.nio.file.Files.deleteIfExists(f): Unit
  }

  test("sort: a NATIVE Spark sortByKey, over the same Heap Tables.via threads") {
    val h0 = Heap.empty[SparkBulk.Rows]
    val (t, h1) = h0.put(Tables.Plan.Of(Vector(5, 3, 1, 4, 2)))
    val prog: Free[Sort, Table[Int]] = t.sortBy((x: Int) => x)
    val (finalHeap, sortedTable) = State.run(h1)(B.sort[Table[Int], Pure](prog))
    val c = new Ctx[SparkBulk.Rows](B, hh => tt => hh.plan(tt), true, (_: Tables.Plan[_]) => ())
    val got = B.toChunks(c.force(finalHeap, sortedTable)).elements.toVector
    assertEquals(got, Vector(1, 2, 3, 4, 5))
  }
}
