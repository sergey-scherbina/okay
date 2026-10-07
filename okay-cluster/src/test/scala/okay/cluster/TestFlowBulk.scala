package okay.cluster

import okay.*
import okay.freer.*

import okay.given
import okay.Tables.read
import okay.Chunks.elements
import scala.util.Random

/**
 * `FlowBulk` (specs/streams-seam.md, lane 1): our engine as a `Bulk`
 * instance. The law is AGREEMENT — a program that names no platform
 * answers on the engine what it answers in one JVM.
 */
class TestFlowBulk extends munit.FunSuite {

  /** two "files" in memory, read by name */
  private val files = Map(
    "big.csv" -> ("k,b\n" + (1 to 300).map(i => s"${i % 7},b$i").mkString("\n") + "\n"),
    "small.csv" -> "k,s\n1,one\n2,two\n3,three\n")
  private val lines: String => Iterator[String] = p => files(p).linesIterator
  private val sizes: String => Option[Long] = p => files.get(p).map(_.length.toLong)

  private val local: Bulk[Chunks] = Bulk.local(lines, sizes)
  private def engine(parts: Int): Bulk[Flow] = FlowBulk(parts, lines, sizes)

  /** TestPlan's job: two CSVs joined, selected, collected */
  private def job[D[_]](B: Bulk[D]): Vector[(String, String, String)] =
    Tables.run(B):
      val small = read("small.csv").select(r => r("k") -> r("s"))
      val big = read("big.csv").select(r => r("k") -> r("b"))
      big.join(small).select { case (k, (b, s)) => (k, b, s) }.collect.map(_.elements.toVector.sorted)

  test("a Tables program answers on the engine what it answers in one JVM") {
    val expected = job(local)
    assert(expected.nonEmpty)
    for parts <- List(1, 4) do assertEquals(job(engine(parts)), expected, s"parts $parts")
  }

  test("of, map, flatMap, filter, join, aggregate, toChunks agree with the local instance, on random and on empty input") {
    for seed <- 1 to 8; parts <- List(1, 4) do
      val rnd = Random(seed)
      val xs = List.fill(rnd.nextInt(200))(rnd.nextInt(50))
      val ys = List.fill(rnd.nextInt(40))((rnd.nextInt(50), rnd.alphanumeric.take(3).mkString))
      def pipeline[D[_]](B: Bulk[D]): (Vector[(Int, (Int, String))], Long, Vector[Int]) =
        val l = B.filter(B.flatMap(B.map(B.of(xs))(_ + 1))(x => List(x, x * 2)))(_ % 3 != 0)
        val joined = B.join(B.map(l)(x => (x, x)), B.of(ys))
        val sum = B.aggregate(l)(Aggregator.sum[Long].contramap[Int](_.toLong))
        (B.toChunks(joined).elements.toVector.sorted, sum, B.toChunks(l).elements.toVector.sorted)
      assertEquals(pipeline(engine(parts)), pipeline(local), s"seed $seed parts $parts")
  }

  test("read reads every split once, one partition per slice; toChunks re-reads per run; cache reads once") {
    val reads = scala.collection.mutable.ArrayBuffer.empty[Int]
    val eightSplits = new Bulk.Format[Int]:
      def name = "eight"
      def splits(path: String): Vector[Int] = Vector.range(0, 8)
      def read(path: String, split: Int): Iterator[Int] = { reads.synchronized { reads += split; () }; Iterator.range(split * 10, split * 10 + 10) }
    val B = engine(4)
    val d = B.read("f", eightSplits)
    assertEquals(B.toChunks(d).elements.toVector, Vector.range(0, 80))
    assertEquals(reads.sorted.toVector, Vector.range(0, 8), "every split once")
    reads.clear()
    assertEquals(B.toChunks(d).elements.size, 80)
    assertEquals(reads.size, 8, "a second consumption reads the file again")
    reads.clear()
    val c = B.cache(d)
    assertEquals(reads.size, 8, "cache reads now")
    assertEquals(B.toChunks(c).elements.size, 80)
    assertEquals(B.toChunks(c).elements.size, 80)
    assertEquals(reads.size, 8, "and never again")
  }

  test("a join reads its right side once per run; a broadcastJoin collects it once for good") {
    val pulls = java.util.concurrent.atomic.AtomicInteger(0)
    val counting = new Bulk.Format[(Int, String)]:
      def name = "counting"
      def splits(path: String): Vector[Int] = Vector(0)
      def read(path: String, split: Int): Iterator[(Int, String)] = { pulls.incrementAndGet(); Iterator.tabulate(10)(i => (i, s"r$i")) }
    val B = FlowBulk(4)
    val joined = B.join(B.map(B.of(List.range(0, 100)))(i => (i % 10, i)), B.read("r", counting))
    assertEquals(B.toChunks(joined).elements.size, 100)
    assertEquals(B.toChunks(joined).elements.size, 100)
    assertEquals(pulls.get, 2, "the exchanged join reads its right side once per run")
    pulls.set(0)
    val broadcast = B.broadcastJoin(B.map(B.of(List.range(0, 100)))(i => (i % 10, i)), B.read("r", counting))
    assertEquals(B.toChunks(broadcast).elements.size, 100)
    assertEquals(B.toChunks(broadcast).elements.size, 100)
    assertEquals(pulls.get, 1, "the broadcast right side was read more than once")
  }

  test("csv prunes at the parser as the local instance does") {
    val B = engine(2)
    val pruned = B.toChunks(B.csv("big.csv", Some(Set("k")))).elements.take(2).toVector
    assertEquals(pruned, Vector(Map("k" -> "1"), Map("k" -> "2")))
    assertEquals(B.toChunks(B.csv("small.csv")).elements.toVector, local.csv("small.csv").elements.toVector)
    assertEquals(B.size("big.csv"), sizes("big.csv"))
  }

  test("a chain of joins runs on the engine: a joined side feeds the next join through a boundary") {
    val a = Vector.tabulate(200)(i => (i % 10, i))
    val b = Vector.tabulate(10)(k => (k, s"b$k"))
    val c = Vector.tabulate(10)(k => (s"b$k", k * 100))
    def prog[D[_]](B: Bulk[D]): Vector[(String, (Int, Int))] =
      val ab = B.map(B.join(B.of(a), B.of(b)))({ case (_, (i, s)) => (s, i) })
      B.toChunks(B.join(ab, B.of(c))).elements.toVector.sorted
    for parts <- List(1, 4) do assertEquals(prog(engine(parts)), prog(local), s"parts $parts")
  }
}

