package okay.persist

import munit.FunSuite
import okay.codec.Schema

/** the evolution fixture: v1 had only the id, v2 adds a count */
final case class EvV1(id: String)
final case class Ev(id: String, n: Int)
object Ev:
  given Schema[EvV1] = Schema.derived
  given Schema[Ev] = Schema.derived
import Ev.given

/**
 * The contract every engine must honor (specs/persist.md, Behavior).
 * Written against the trait so the file engine runs the same suite —
 * a consumer developed against memory meets no surprises on disk.
 */
abstract class StoreSuite extends FunSuite:

  /** a fresh store per test */
  def mkStore(): Store

  /** a policy tight enough that a handful of appends triggers
   * retention (engine-specific granularity) */
  def tinyRetention: Policy

  /** a compacted-topic policy under which ~30 small appends leave
   * the engine something to actually compact (the file engine
   * compacts closed segments only, so it needs segments to roll);
   * the tiny retainBytes must be IGNORED — compaction and retention
   * are exclusive */
  def tinyCompact: Policy = Policy(compact = true, retainBytes = 100)

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")
  private def str(b: Array[Byte]): String = new String(b, "UTF-8")

  private def records(r: Topic.Read): Vector[Record] = r match
    case Topic.Read.Records(rs) => rs
    case Topic.Read.TooEarly(b) => fail(s"unexpected TooEarly($b)")

  test("append then read: order, dense offsets, begin and end") {
    val t = mkStore().topic("events")
    val offs = (0 until 5).map(i => t.append(0, bytes(s"k$i"), bytes(s"v$i"), Ack.Durable))
    assertEquals(offs.toVector, Vector(0L, 1L, 2L, 3L, 4L))
    assertEquals(t.begin(0), 0L)
    assertEquals(t.end(0), 5L)
    val rs = records(t.read(0, 0L, 100))
    assertEquals(rs.map(_.offset), Vector(0L, 1L, 2L, 3L, 4L))
    assertEquals(rs.map(r => str(r.value)), Vector("v0", "v1", "v2", "v3", "v4"))
    assertEquals(rs.map(r => str(r.key)), Vector("k0", "k1", "k2", "k3", "k4"))
  }

  test("poll-on-end: a caught-up reader sees a later append") {
    // the line tailing stands on (ui-durable, resumable SSE): read to
    // `end`, park, and the next read from there sees what arrived
    val t = mkStore().topic("tail")
    (0 until 3).foreach(i => t.append(0, Array.empty, bytes(s"v$i"), Ack.Durable))
    val caughtUp = t.end(0)
    assertEquals(records(t.read(0, caughtUp, 10)), Vector.empty)
    t.append(0, Array.empty, bytes("late"), Ack.Durable)
    assertEquals(records(t.read(0, caughtUp, 10)).map(r => str(r.value)), Vector("late"))
    assertEquals(t.end(0), caughtUp + 1)
  }

  test("read from the middle, bounded by max; read at end is empty") {
    val t = mkStore().topic("events")
    (0 until 10).foreach(i => t.append(0, Array.empty, bytes(s"v$i"), Ack.Received))
    assertEquals(records(t.read(0, 4L, 3)).map(_.offset), Vector(4L, 5L, 6L))
    assertEquals(records(t.read(0, 10L, 3)), Vector.empty)
  }

  test("keyed appends: same key, same partition, order kept under interleaving") {
    val t = mkStore().topic("sessions", partitions = 4)
    // two writers interleaved over the keyed convenience
    val keys = Vector("alice", "bob")
    for i <- 0 until 20; k <- keys do t.append(bytes(k), bytes(s"$k-$i"))
    for k <- keys do
      val p = Topic.route(bytes(k), 4)
      val mine = records(t.read(p, 0L, 1000))
        .filter(r => str(r.key) == k).map(r => str(r.value))
      assertEquals(mine, (0 until 20).map(i => s"$k-$i").toVector)
  }

  test("routing is pure and in range") {
    for k <- Vector("", "a", "alice", "\u0000\u00ff", "long".repeat(100)) do
      val p = Topic.route(bytes(k), 7)
      assert(p >= 0 && p < 7)
      assertEquals(p, Topic.route(bytes(k), 7))
  }

  test("retention: begin advances, reading before it says TooEarly, not silence") {
    val t = mkStore().topic("bounded", partitions = 1, policy = tinyRetention)
    (0 until 50).foreach(i => t.append(0, Array.empty, bytes(s"payload-$i"), Ack.Durable))
    val b = t.begin(0)
    assert(b > 0L, s"retention never moved begin (begin=$b)")
    t.read(0, 0L, 10) match
      case Topic.Read.TooEarly(at) => assertEquals(at, b)
      case Topic.Read.Records(rs) => fail(s"expected TooEarly, got ${rs.length} records")
    // everything from begin is still there, still dense
    val rs = records(t.read(0, b, 1000))
    assertEquals(rs.map(_.offset), (b until 50L).toVector)
  }

  test("a topic reopened with a different partition count is refused") {
    val s = mkStore()
    val _ = s.topic("fixed", partitions = 3)
    intercept[IllegalArgumentException](s.topic("fixed", partitions = 5))
  }

  test("stats: begin, end, bytes, segments per partition") {
    val s = mkStore()
    val t = s.topic("observed", partitions = 2)
    (0 until 6).foreach(i => t.append(i % 2, Array.empty, bytes(s"v$i"), Ack.Durable))
    val st = s.stats.topics.find(_.name == "observed").get
    assertEquals(st.partitions.length, 2)
    for p <- st.partitions do
      assertEquals(p.begin, 0L)
      assertEquals(p.end, 3L)
      assert(p.bytes > 0L)
      assert(p.segments > 0)
  }

  test("compaction keeps the latest record per key; a refold from begin equals the fold of full history") {
    val t = mkStore().topic("kv", partitions = 1, policy = tinyCompact)
    val keys = Vector("a", "b", "c")
    for i <- 0 until 30 do t.append(0, bytes(keys(i % 3)), bytes(s"$i"), Ack.Durable)
    // retention must NOT have run on a compacted topic, despite the
    // tiny retainBytes: dropping from the front deletes quiet keys
    assertEquals(t.begin(0), 0L, "retention ran on a compacted topic")

    def foldOf(rs: Vector[Record]): Map[String, String] =
      rs.foldLeft(Map.empty[String, String])((m, r) => m.updated(str(r.key), str(r.value)))
    val before = records(t.read(0, 0L, 100))
    val full = foldOf(before)

    t.compact(0)

    val after = records(t.read(0, t.begin(0), 100))
    assert(after.length < before.length, "compaction dropped nothing")
    assertEquals(foldOf(after), full, "the refold of the compacted partition diverged")
    // offsets are preserved (holes, not renumbering) and stay ordered
    assertEquals(after.map(_.offset), after.map(_.offset).sorted)
    assert(after.forall(r => before.exists(b => b.offset == r.offset && str(b.value) == str(r.value))))
    assertEquals(t.begin(0), 0L)
    assertEquals(t.end(0), 30L, "compaction moved end")
    // the sequence continues densely after compaction
    assertEquals(t.append(0, bytes("d"), bytes("30"), Ack.Durable), 30L)
  }

  test("typed view: Schema at the edge, damage is data naming the offset") {
    val t = mkStore().topic("typed", partitions = 1)
    val v = t.of[Ev]()
    v.append(0, bytes("k"), Ev("x", 1), Ack.Durable)
    // a raw, unenveloped record lands between the typed ones
    t.append(0, bytes("k"), bytes("garbage-bytes"), Ack.Durable)
    t.append(0, bytes("k"), Array[Byte](1, 2), Ack.Durable)
    v.append(0, bytes("k"), Ev("y", 2), Ack.Durable)

    v.read(0, 0L, 10) match
      case Typed.Read.TooEarly(b) => fail(s"unexpected TooEarly($b)")
      case Typed.Read.Records(ds) =>
        assertEquals(ds.length, 4)
        ds(0) match
          case Typed.Decoded.Ok(0L, _, _, a) => assertEquals(a, Ev("x", 1))
          case other => fail(s"expected Ok at 0, got $other")
        ds(1) match
          case Typed.Decoded.Bad(1L, e) => assert(e.nonEmpty)
          case other => fail(s"expected Bad at 1, got $other")
        ds(2) match
          case Typed.Decoded.Bad(2L, e) => assert(e.contains("envelope"), e)
          case other => fail(s"expected Bad at 2, got $other")
        ds(3) match
          case Typed.Decoded.Ok(3L, _, _, a) => assertEquals(a, Ev("y", 2))
          case other => fail(s"expected Ok at 3, got $other")
  }

  test("a v1 record reads under v2 through the upcast; an unknown version is an error value") {
    val t = mkStore().topic("evolving", partitions = 1)
    Typed[EvV1](t, version = 1, upcasts = Map.empty).append(0, bytes("k"), EvV1("old"), Ack.Durable)
    val v2 = Typed[Ev](t, version = 2,
      upcasts = Map(1 -> Typed.step[EvV1, Ev](o => Ev(o.id, 0))))
    v2.append(0, bytes("k"), Ev("new", 5), Ack.Durable)
    // a record from the future, and one this reader has no road to
    t.append(0, bytes("k"), Typed.seal(3, Array[Byte](0x60.toByte)), Ack.Durable)

    val ds = v2.read(0, 0L, 10) match
      case Typed.Read.Records(ds) => ds
      case Typed.Read.TooEarly(b) => fail(s"unexpected TooEarly($b)")
    ds(0) match
      case Typed.Decoded.Ok(0L, _, _, a) => assertEquals(a, Ev("old", 0))
      case other => fail(s"the upcast road failed: $other")
    ds(1) match
      case Typed.Decoded.Ok(1L, _, _, a) => assertEquals(a, Ev("new", 5))
      case other => fail(s"the current version failed: $other")
    ds(2) match
      case Typed.Decoded.Bad(2L, e) =>
        assert(e.contains("version 3") && e.contains("2"), e)
      case other => fail(s"the future version must be an error value, got $other")

    // an old version with NO upcast on the road is an error, not a guess
    val noRoad = Typed[Ev](t, version = 2, upcasts = Map.empty)
    noRoad.read(0, 0L, 1) match
      case Typed.Read.Records(Vector(Typed.Decoded.Bad(0L, e))) =>
        assert(e.contains("no upcast"), e)
      case other => fail(s"expected Bad(no upcast), got $other")
  }

  test("a consumer commits offsets and resumes from its commit after a restart") {
    val s = mkStore()
    val t = s.topic("events")
    (0 until 10).foreach(i => t.append(0, Array.empty, bytes(s"v$i"), Ack.Durable))

    val first = Offsets(s)
    assertEquals(first.committed("g", "events", 0), None)
    assertEquals(first.lag("g", t), 10L)
    first.commit("g", "events", 0, 7L)
    assertEquals(first.committed("g", "events", 0), Some(7L))
    assertEquals(first.lag("g", t), 3L)

    // the restart: a fresh instance folds the offsets topic anew
    val second = Offsets(s)
    assertEquals(second.committed("g", "events", 0), Some(7L),
      "the commit did not survive the restart")
    second.commit("g", "events", 0, 10L)
    assertEquals(Offsets(s).lag("g", t), 0L)
    // groups are independent
    assertEquals(second.committed("other", "events", 0), None)
  }

  test("snapshots: put and latest over a compacted keyed topic") {
    val s = mkStore()
    val sn = Snapshots(s)
    assertEquals(sn.latest(bytes("sess")), None)
    sn.put(bytes("sess"), bytes("s1"))
    val off = sn.put(bytes("sess"), bytes("s2"))
    sn.put(bytes("other"), bytes("o1"))

    val r = sn.latest(bytes("sess")).getOrElse(fail("no snapshot"))
    assertEquals(r.offset, off)
    assertEquals(str(r.value), "s2")
    // compaction reclaims the superseded state, the latest survives
    sn.topic.compact(0)
    assertEquals(sn.latest(bytes("sess")).map(x => str(x.value)), Some("s2"))
    // the Schema'd pair, for consumers whose state has one
    sn.putValue(bytes("n"), Ev("z", 9))
    assertEquals(sn.latestValue[Ev](bytes("n")).map(_._2), Some(Right(Ev("z", 9))))
  }

class TestMemoryStore extends StoreSuite:
  def mkStore(): Store = MemoryStore()
  // memory counts a frame as bytes + 28 overhead; ~9 records fit
  def tinyRetention: Policy = Policy(retainBytes = 340)
