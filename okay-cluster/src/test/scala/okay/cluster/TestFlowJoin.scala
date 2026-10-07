package okay.cluster


import okay.{Pane, Streamed, Tables, Bulk}
import okay.freer.*
import okay.std.*
import okay.given
import okay.freer.given
import okay.std.given
import okay.Chunks.elements
import okay.freer.Row.plus
import okay.Streamed.{joinSorted, joinWithin, windowed, zip}
import okay.Tables.collect
import scala.util.Random

/**
 * `Flow.Join` and `FlowBulk.streamed` (specs/streams-seam.md, lane 2):
 * the engine's co-partitioned join, and the `Streamed` signatures
 * answered natively. The law is AGREEMENT with the local machines and
 * with `Streamed.viaTables`.
 */
class TestFlowJoin extends munit.FunSuite {

  private def keyed(rnd: Random, n: Int, tag: String): Vector[(Int, String)] =
    Vector.fill(n)((rnd.nextInt(12), s"$tag${rnd.nextInt(1000)}"))
  private def flow[A](xs: Vector[A], parts: Int): Flow[A] = Flow.slices(xs, parts)
  private def collectAll[A](f: Flow[A]): Vector[A] = Flows.collect(f).runWith

  test("the hash join answers the local hash join at 1 and 4 buckets, over 1 and 3 partitions a side") {
    for seed <- 1 to 6; buckets <- List(1, 4); parts <- List(1, 3) do
      val rnd = Random(seed)
      val l = keyed(rnd, rnd.nextInt(80), "l"); val r = keyed(rnd, rnd.nextInt(80), "r")
      val expected = (for (k, a) <- l; (k2, b) <- r if k == k2 yield (k, (a, b))).sorted
      assertEquals(collectAll(flow(l, parts).join(flow(r, parts), buckets)).sorted, expected, s"seed $seed buckets $buckets parts $parts")
  }

  test("the sort-merge join answers the same on key-ordered sides sliced into partitions; an unordered side fails by name") {
    for seed <- 1 to 6; buckets <- List(1, 4); parts <- List(1, 3) do
      val rnd = Random(seed)
      val l = keyed(rnd, rnd.nextInt(80), "l").sortBy(_._1); val r = keyed(rnd, rnd.nextInt(80), "r").sortBy(_._1)
      val expected = (for (k, a) <- l; (k2, b) <- r if k == k2 yield (k, (a, b))).sorted
      assertEquals(collectAll(flow(l, parts).joinSorted(flow(r, parts), buckets)).sorted, expected, s"seed $seed buckets $buckets parts $parts")
    // the violation on the side still being read: an inner join ends at the other side's end
    val e = intercept[IllegalArgumentException](collectAll(flow(Vector((1, "a"), (3, "c")), 1).joinSorted(flow(Vector((3, "x"), (1, "y")), 1), 1)))
    assert(e.getMessage.contains("not sorted"), e.getMessage)
  }

  test("the windowed join answers the interval predicate at any buckets and partitions") {
    for seed <- 1 to 6; buckets <- List(1, 4); parts <- List(1, 3) do
      val rnd = Random(seed)
      val l = Vector.fill(rnd.nextInt(60))((rnd.nextInt(5), (rnd.nextLong(100), s"l${rnd.nextInt(1000)}")))
      val r = Vector.fill(rnd.nextInt(60))((rnd.nextInt(5), (rnd.nextLong(100), s"r${rnd.nextInt(1000)}")))
      val expected = (for (k, a) <- l; (k2, b) <- r if k == k2 && math.abs(a._1 - b._1) <= 7L yield (k, (a, b))).sorted
      assertEquals(collectAll(flow(l, parts).joinWithin(flow(r, parts), buckets, 7L, 0L)(_._1, _._1)).sorted, expected, s"seed $seed buckets $buckets parts $parts")
  }

  test("a keyed input into a join is refused by name") {
    val k = flow(Vector((1, 2L)), 2).keyBy(_._1)(Aggregator.sum[Long].contramap[(Int, Long)](_._2))
    val e = intercept[IllegalArgumentException](collectAll(k.join(flow(Vector((1, "x")), 1), 2)))
    assert(e.getMessage.contains("a join's left side follows another keyed stage"), e.getMessage)
  }

  test("a Tables + Streamed program answers on the engine what viaTables answers in one JVM") {
    final case class Ev(ts: Long, key: String, v: Long)
    val rnd = Random(3)
    val l = keyed(rnd, 70, "l").sortBy(_._1); val r = keyed(rnd, 50, "r").sortBy(_._1)
    val tl = Vector.fill(60)((rnd.nextInt(5), (rnd.nextLong(100), s"l${rnd.nextInt(1000)}")))
    val tr = Vector.fill(60)((rnd.nextInt(5), (rnd.nextLong(100), s"r${rnd.nextInt(1000)}")))
    val evs = Vector.fill(300)(Ev(rnd.nextLong(2000), (rnd.nextInt(3) + 'a').toChar.toString, rnd.nextInt(10).toLong))
    val sum = Aggregator.sum[Long].contramap[Ev](_.v)
    type R = Tables + Streamed
    def prog: (Vector[(Int, (String, String))], Vector[(Int, ((Long, String), (Long, String)))], Vector[Pane[String, Long]], Vector[(Int, String)]) ! R =
      for
        js <- Tables.of(l).plus[Streamed].joinSorted(Tables.of(r).plus[Streamed]).collect.map(_.elements.toVector.sorted)
        jw <- Tables.of(tl).plus[Streamed].joinWithin(Tables.of(tr).plus[Streamed], 7L, 0L)(_._1, _._1).collect.map(_.elements.toVector.sorted)
        w <- Tables.of(evs).plus[Streamed].windowed(200L, 100L, 30L)(_.key)(_.ts)(sum).collect.map(_.elements.toVector.sortBy(p => (p.start, p.key)))
        z <- Tables.of(List(1, 2, 3)).plus[Streamed].zip(Tables.of(List("a", "b")).plus[Streamed]).collect.map(_.elements.toVector)
      yield (js, jw, w, z)
    val local = Tables.run(Bulk.local(_ => Iterator.empty))(Streamed.viaTables(prog))
    for parts <- List(1, 4) do
      assertEquals(FlowBulk(parts).run(prog), local, s"parts $parts")
  }
}
