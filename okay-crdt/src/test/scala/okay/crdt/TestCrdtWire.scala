package okay.crdt

import okay.{Hlc, Uid}
import okay.codec.{Json, Schema}
import okay.crdt.Wire.given

/**
 * Three laws for the wire, and they are not the same three as stage
 * 2's. Those were about `merge`; these are about whether shipping a
 * replica changes it.
 *
 *   1. equal values encode to EQUAL BYTES
 *   2. decode(encode(x)) == x
 *   3. merging two DECODED replicas equals decoding the MERGE
 *
 * The third is the one that would catch a lossy encoding — an
 * encoding that drops a tombstone or a per-node count would still
 * round-trip a single value and still be canonical, and would break
 * convergence the moment two replicas met.
 */
class TestCrdtWire extends munit.FunSuite {

  private val a = NodeId("alice")
  private val b = NodeId("bob")
  private val c = NodeId("carol")

  private def roundTrips[A](x: A)(using s: Schema[A]): Unit =
    assertEquals(Json.read[A](Json.write(x)), Right(x), s"round trip: ${Json.write(x)}")

  private def sameBytes[A](x: A, y: A)(using Schema[A]): Unit =
    assertEquals(x, y, "the test itself is wrong if these are not equal values")
    assertEquals(Json.write(x), Json.write(y),
      s"equal values encoded differently:\n  ${Json.write(x)}\n  ${Json.write(y)}")

  /** merging after the wire equals merging before it */
  private def mergeSurvives[A](x: A, y: A)(using s: Schema[A], m: Crdt[A]): Unit =
    val there = Json.read[A](Json.write(m.merge(x, y)))
    val back = for
      dx <- Json.read[A](Json.write(x))
      dy <- Json.read[A](Json.write(y))
    yield m.merge(dx, dy)
    assertEquals(back, there, "the wire changed what a merge answers")

  // ── 1 and 2, per type ───────────────────────────────────────────

  test("GCounter: round trips, and two build orders give the same bytes") {
    val x = GCounter.empty.inc(a, 2).inc(b).inc(c, 5)
    val y = GCounter.empty.inc(c, 5).inc(b).inc(a, 2)   // the other insertion order
    roundTrips(x)
    sameBytes(x, y)
    roundTrips(GCounter.empty)
  }

  test("PNCounter: round trips, both halves, either build order") {
    val x = PNCounter.empty.inc(a, 4).dec(b, 1).inc(c)
    val y = PNCounter.empty.inc(c).dec(b, 1).inc(a, 4)
    roundTrips(x)
    sameBytes(x, y)
    assertEquals(Json.read[PNCounter](Json.write(x)).map(_.value), Right(4L))
  }

  test("GSet: round trips, and set order does not reach the wire") {
    val x = GSet(Set("pear", "apple", "quince"))
    val y = GSet(Set("quince", "apple", "pear"))
    roundTrips(x)
    sameBytes(x, y)
    roundTrips(GSet.empty[String])
  }

  test("OrSet: round trips, with both the adds and each element's TAGS sorted") {
    val gen = Uid.at(() => 1_700_000_000_000L)
    val t1 = gen.next(); val t2 = gen.next(); val t3 = gen.next()
    // the same value assembled two ways: different insertion order at
    // BOTH levels, since one unsorted level is enough to break this
    val x = OrSet.empty[String].add("b", t2).add("a", t1).add("a", t3)
    val y = OrSet.empty[String].add("a", t3).add("a", t1).add("b", t2)
    roundTrips(x)
    sameBytes(x, y)
    val removed = x.remove("a")
    roundTrips(removed)
    assert(Json.write(removed).contains(t1.ulid), "a tombstone must survive the wire")
  }

  test("LwwRegister: round trips, stamp and node intact") {
    val clock = Hlc.at(() => 1_700_000_000_000L)
    val r = LwwRegister.write("hello", clock, a)
    roundTrips(r)
    val back = Json.read[LwwRegister[String]](Json.write(r))
    assertEquals(back.map(_.at.toLong), Right(r.at.toLong), "the stamp must survive exactly")
    assertEquals(back.map(_.by.name), Right("alice"))
  }

  test("Uid and NodeId travel as text, and a Uid sorts as its text does") {
    val gen = Uid.at(() => 1_700_000_000_000L)
    val u = gen.next()
    assertEquals(Json.write(u), "\"" + u.ulid + "\"")
    assertEquals(Json.read[Uid](Json.write(u)), Right(u))
    assertEquals(Json.read[Uid]("\"not-a-ulid\"").isLeft, true, "garbage refuses")
    assertEquals(Json.read[NodeId](Json.write(a)), Right(a))
  }

  // ── 3: the law that catches a lossy encoding ────────────────────

  test("THE MERGE SURVIVES THE WIRE: GCounter") {
    mergeSurvives(GCounter.empty.inc(a, 3), GCounter.empty.inc(b, 4).inc(a))
  }

  test("THE MERGE SURVIVES THE WIRE: PNCounter") {
    mergeSurvives(PNCounter.empty.inc(a, 3).dec(b), PNCounter.empty.dec(a, 1))
  }

  test("THE MERGE SURVIVES THE WIRE: GSet") {
    mergeSurvives(GSet(Set("x", "y")), GSet(Set("y", "z")))
  }

  test("THE MERGE SURVIVES THE WIRE: OrSet, including its tombstones") {
    // the case that would expose a dropped tombstone: one replica
    // removed what the other never saw removed
    val gen = Uid.at(() => 1_700_000_000_000L)
    val start = OrSet.empty[String].add("x", gen.next()).add("y", gen.next())
    mergeSurvives(start.remove("x"), start.add("x", gen.next()))
  }

  test("THE MERGE SURVIVES THE WIRE: LwwRegister across two clocks") {
    val ca = Hlc.at(() => 1_700_000_000_000L)
    val cb = Hlc.at(() => 1_700_000_000_000L - 3_600_000L)
    val first = LwwRegister.write("alice's", ca, a)
    mergeSurvives(first, LwwRegister.after("bob's", first, cb, b))
  }

  test("a replica that has converged ships the same bytes however it got there") {
    // two replicas that merged the same three updates in different
    // orders: equal by stage 2's laws, and now equal on the wire too
    val m = summon[Crdt[GCounter]]
    val u1 = GCounter.empty.inc(a, 2)
    val u2 = GCounter.empty.inc(b, 3)
    val u3 = GCounter.empty.inc(c)
    val left = m.merge(m.merge(u1, u2), u3)
    val right = m.merge(u3, m.merge(u2, u1))
    sameBytes(left, right)
  }
}
