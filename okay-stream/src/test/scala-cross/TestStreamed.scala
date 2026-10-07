package okay


import okay.freer.*


import scala.util.Random
import Chunks.elements
import Row.plus
import Streamed.{joinSorted, joinWithin, windowed, zip}
import Tables.{join, collect}

/**
 * `Streamed` (specs/streams-seam.md, lane 2): the stream operators as a
 * signature in the row, and `viaTables`, the platform-free answer. Each
 * law compares the signature's answer with the local machine it names.
 */
class TestStreamed extends munit.FunSuite {

  private val B: Bulk[Chunks] = Bulk.local(_ => Iterator.empty)
  private type R = Tables + Streamed
  private def run[A](p: A ! R): A = Tables.run(B)(Streamed.viaTables(p))

  private def keyed(rnd: Random, n: Int, tag: String): Vector[(Int, String)] =
    Vector.fill(n)((rnd.nextInt(12), s"$tag${rnd.nextInt(1000)}"))

  test("joinSorted answers the hash join's multiset on key-ordered sides") {
    for seed <- 1 to 10 do
      val rnd = Random(seed)
      val l = keyed(rnd, rnd.nextInt(60), "l").sortBy(_._1)
      val r = keyed(rnd, rnd.nextInt(60), "r").sortBy(_._1)
      val sorted = run(Tables.of(l).plus[Streamed].joinSorted(Tables.of(r).plus[Streamed]).collect.map(_.elements.toVector.sorted))
      val hashed = Tables.run(B)(Tables.of(l).join(Tables.of(r)).collect.map(_.elements.toVector.sorted))
      assertEquals(sorted, hashed, s"seed $seed")
  }

  test("joinWithin answers the interval predicate, whatever the sides' order; lateness changes nothing on a table") {
    for seed <- 1 to 10; lateness <- List(0L, 5L, 1000L) do
      val rnd = Random(seed)
      val l = Vector.fill(rnd.nextInt(50))((rnd.nextInt(5), (rnd.nextLong(100), s"l${rnd.nextInt(1000)}")))
      val r = Vector.fill(rnd.nextInt(50))((rnd.nextInt(5), (rnd.nextLong(100), s"r${rnd.nextInt(1000)}")))
      val within = 7L
      val got = run(Tables.of(l).plus[Streamed].joinWithin(Tables.of(r).plus[Streamed], within, lateness)(_._1, _._1)
        .collect.map(_.elements.toVector.sorted))
      val expected = (for (k, a) <- l; (k2, b) <- r if k == k2 && math.abs(a._1 - b._1) <= within yield (k, (a, b))).sorted
      assertEquals(got, expected, s"seed $seed lateness $lateness")
  }

  test("windowed answers Windows' panes over the table's order, late rows dropped as there") {
    final case class Ev(ts: Long, key: String, v: Long)
    val rnd = Random(7)
    val evs = Vector.fill(400)(Ev(rnd.nextLong(2000), (rnd.nextInt(3) + 'a').toChar.toString, rnd.nextInt(10).toLong))
    val sum = Aggregator.sum[Long].contramap[Ev](_.v)
    val got = run(Tables.of(evs).plus[Streamed].windowed(200L, 100L, 30L)(_.key)(_.ts)(sum).collect.map(_.elements.toVector))
    val w = new Windows[String, Ev, Long, Long](200L, 100L, 30L, _.key, _.ts, sum)
    val expected = Vector.newBuilder[Pane[String, Long]]
    for e <- evs do w.add(e)(p => { expected += p; () })
    w.close()(p => { expected += p; () })
    assertEquals(got.sortBy(p => (p.start, p.key)), expected.result().sortBy(p => (p.start, p.key)))
    assert(w.dropped > 0, "the sample should have late rows for the law to mean anything")
  }

  test("zip is Chunks.zip: positional, ending at the shorter side") {
    val got = run(Tables.of(List(1, 2, 3)).plus[Streamed].zip(Tables.of(List("a", "b")).plus[Streamed]).collect.map(_.elements.toVector))
    assertEquals(got, Vector((1, "a"), (2, "b")))
  }
}
