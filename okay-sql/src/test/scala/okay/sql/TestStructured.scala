package okay.sql


import okay.{Bulk, Chunks, Tables}
import okay.freer.*

import okay.codec.Schema
import okay.freer.Row.plus
import okay.Chunks.elements
import okay.Tables.{collect, select}
import okay.sql.Structured.{matching, joinOn}
import scala.util.Random

/**
 * `Structured` (specs/streams-seam.md, lane 5): `matching` and `joinOn`
 * as data, answered by `viaTables` on any platform. The law: the same
 * answer as the opaque filter and join written by hand.
 */
class TestStructured extends munit.FunSuite {
  final case class Trip(id: Long, route: String, service: String, tram: Boolean)
  final case class Stop(trip: Long, time: String, seq: Int)
  given Schema[Trip] = Schema.derived
  given Schema[Stop] = Schema.derived

  val route = Query.field[Trip, String]("route").toOption.get
  val tram = Query.field[Trip, Boolean]("tram").toOption.get
  val tripId = Query.field[Trip, Long]("id").toOption.get
  val stopTrip = Query.field[Stop, Long]("trip").toOption.get
  val seq = Query.field[Stop, Int]("seq").toOption.get

  private val B: Bulk[Chunks] = Bulk.local(_ => Iterator.empty)
  private type R = Tables + Structured
  private def run[A](p: A ! R): A = Tables.run(B)(Structured.viaTables(p))

  private def data(seed: Int): (Vector[Trip], Vector[Stop]) =
    val rnd = Random(seed)
    val trips = Vector.tabulate(40)(i => Trip(i.toLong, s"r${rnd.nextInt(6)}", s"s${rnd.nextInt(3)}", rnd.nextBoolean()))
    val stops = Vector.fill(300)(Stop(rnd.nextInt(50).toLong, f"${rnd.nextInt(24)}%02d:${rnd.nextInt(60)}%02d", rnd.nextInt(30)))
    (trips, stops)

  test("matching answers the hand-written filter") {
    for seed <- 1 to 8 do
      val (trips, _) = data(seed)
      val w = (route === "r1" or route === "r2") and (tram === true)
      val got = run(Tables.of(trips).plus[Structured].matching(w).collect.map(_.elements.toVector))
      assertEquals(got, trips.filter(t => (t.route == "r1" || t.route == "r2") && t.tram), s"seed $seed")
  }

  test("joinOn answers the hand-written key join, a missing key dropping on both sides") {
    for seed <- 1 to 8 do
      val (trips, stops) = data(seed)
      val got = run(Tables.of(stops).plus[Structured].joinOn(Tables.of(trips).plus[Structured])(stopTrip, tripId)
        .collect.map(_.elements.toVector.sortBy((s, _) => (s.trip, s.seq, s.time))))
      val expected = (for s <- stops; t <- trips if s.trip == t.id yield (s, t)).sortBy((s, _) => (s.trip, s.seq, s.time))
      assertEquals(got, expected, s"seed $seed")
  }

  test("a structural step and an opaque step compose in one program") {
    val (trips, stops) = data(3)
    val got = run(
      Tables.of(stops).plus[Structured].matching(seq < 10)
        .joinOn(Tables.of(trips).plus[Structured].matching(tram === true))(stopTrip, tripId)
        .select((s, t) => (t.route, s.time)).plus[Structured]
        .collect.map(_.elements.toVector.sorted))
    val expected = (for s <- stops if s.seq < 10; t <- trips if t.tram && s.trip == t.id yield (t.route, s.time)).sorted
    assertEquals(got, expected)
  }
}
