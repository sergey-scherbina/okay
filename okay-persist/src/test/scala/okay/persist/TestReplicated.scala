package okay.persist

import munit.FunSuite

/**
 * Stage 2's core (specs/persist.md): quorum acks, the high-water
 * mark, epoch fencing, the producer window — against in-process
 * replicas, because the machinery is transport-agnostic and the
 * wire only moves the replicas out later. A `Pausable` store
 * stands in for the follower that is down: its append throws, its
 * lag shows, `replicate` catches it up.
 */
class TestReplicated extends FunSuite:

  /** a Store whose appends can be switched off — the down replica */
  final class Pausable(inner: Store) extends Store:
    var paused = false
    def topic(name: String, partitions: Int, policy: Policy): Topic =
      val t = inner.topic(name, partitions, policy)
      new Topic:
        def name = t.name
        def partitions = t.partitions
        def append(p: Int, k: Array[Byte], v: Array[Byte], a: Ack): Long =
          if paused then throw IllegalStateException("replica is down")
          else t.append(p, k, v, a)
        def read(p: Int, from: Long, max: Int): Topic.Read = t.read(p, from, max)
        def begin(p: Int): Long = t.begin(p)
        def end(p: Int): Long = t.end(p)
        def compact(p: Int): Unit = t.compact(p)
    def topics: Vector[String] = inner.topics
    def stats: Store.Stats = inner.stats

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")
  private def str(b: Array[Byte]): String = new String(b, "UTF-8")

  def cluster(): (Replicated, Vector[Pausable]) =
    val rs = Vector.fill(3)(Pausable(MemoryStore()))
    (Replicated("events", 1, Policy(), rs.map(r => r: Store)), rs)

  private def records(r: Topic.Read): Vector[Record] = r match
    case Topic.Read.Records(rs) => rs
    case Topic.Read.TooEarly(b) => fail(s"unexpected TooEarly($b)")

  test("replication is a consumer: every replica holds every record; end is the hwm") {
    val (t, rs) = cluster()
    (0 until 5).foreach(i => t.append(0, bytes(s"k$i"), bytes(s"v$i"), Ack.Replicated))
    assertEquals(t.end(0), 5L)
    assertEquals(records(t.read(0, 0, 100)).map(r => str(r.value)),
      (0 until 5).map(i => s"v$i").toVector)
    for r <- rs do assertEquals(r.topic("events", 1, Policy()).end(0), 5L)
    val st = t.replicaStats.partitions.head
    assertEquals(st.hwm, 5L)
    assert(st.replicas.forall(_.lag == 0L))
  }

  test("a down follower is counted out: quorum holds, lag shows, replicate catches up") {
    val (t, rs) = cluster()
    t.append(0, bytes("k"), bytes("v0"), Ack.Replicated)
    rs(2).paused = true
    // 2 of 3 is still a quorum: the ack holds, the lag is visible
    t.append(0, bytes("k"), bytes("v1"), Ack.Replicated)
    assertEquals(t.end(0), 2L)
    val lagging = t.replicaStats.partitions.head.replicas(2)
    assertEquals(lagging.lag, 1L)

    rs(2).paused = false
    t.replicate(0)
    assertEquals(t.replicaStats.partitions.head.replicas(2).lag, 0L)
  }

  test("short of quorum: Replicated refuses loudly, and NOTHING unconfirmed is readable") {
    val (t, rs) = cluster()
    t.append(0, bytes("k"), bytes("v0"), Ack.Replicated)
    rs(1).paused = true
    rs(2).paused = true

    // a Durable append lands on the leader but the hwm cannot move:
    // a reader must not observe what a failover could unwrite
    t.append(0, bytes("k"), bytes("v1"), Ack.Durable)
    assertEquals(t.end(0), 1L, "the hwm advanced without a quorum")
    assertEquals(records(t.read(0, 0, 100)).map(r => str(r.value)), Vector("v0"))

    // and the ack that PROMISES a quorum refuses when there is none
    intercept[Replicated.NoQuorum](
      t.append(0, bytes("k"), bytes("v2"), Ack.Replicated))

    rs(1).paused = false
    rs(2).paused = false
    t.replicate(0)
    assertEquals(t.end(0), 3L)
    assertEquals(records(t.read(0, 0, 100)).map(r => str(r.value)),
      Vector("v0", "v1", "v2"))
  }

  test("a deposed leader's append is fenced, and the rejection is an ops event") {
    val (t, rs) = cluster()
    t.append(0, bytes("k"), bytes("v0"), Ack.Replicated)
    val old = t.leader(0)
    t.promote(0, replica = 1)

    intercept[Replicated.Fenced](t.append(old, bytes("k"), bytes("late"), Ack.Durable))
    // the new epoch's handle writes; history survived the failover
    assertEquals(t.append(t.leader(0), bytes("k"), bytes("v1"), Ack.Replicated), 1L)
    assertEquals(records(t.read(0, 0, 10)).map(r => str(r.value)), Vector("v0", "v1"))

    // the log is its own audit trail: Promoted then FencedAppend
    val ops = rs.head.topic("__ops", 1, Policy())
    val seen = Typed[Replicated.Op](ops, 1, Map.empty).read(0, 0, 10) match
      case Typed.Read.Records(ds) => ds.collect { case Typed.Decoded.Ok(_, _, _, op) => op }
      case other => fail(s"unexpected $other")
    assert(seen.exists {
      case Replicated.Op.Promoted(0, 2L, 0, 1) => true
      case _ => false
    }, seen.toString)
    assert(seen.exists {
      case Replicated.Op.FencedAppend(0, 1L, 2L) => true
      case _ => false
    }, seen.toString)
  }

  test("the idempotent producer: a retried (producerId, seq) lands once, answering the original offset") {
    val (t, _) = cluster()
    val first = t.produce(0, "prod-1", seq = 1, bytes("k"), bytes("v"), Ack.Replicated)
    val retry = t.produce(0, "prod-1", seq = 1, bytes("k"), bytes("v"), Ack.Replicated)
    assertEquals(retry, first)
    assertEquals(t.end(0), 1L, "the retry appended a second record")

    assertEquals(t.produce(0, "prod-1", seq = 2, bytes("k"), bytes("v2"), Ack.Replicated), first + 1)
    intercept[Replicated.ReplayBeyondWindow](
      t.produce(0, "prod-1", seq = 0, bytes("k"), bytes("old"), Ack.Replicated))
    // producers are independent
    assertEquals(t.produce(0, "prod-2", seq = 1, bytes("k"), bytes("w"), Ack.Replicated), 2L)
  }

  test("promotion catches the successor up first: nothing acknowledged is lost") {
    val (t, rs) = cluster()
    rs(1).paused = true
    (0 until 4).foreach(i => t.append(0, bytes("k"), bytes(s"v$i"), Ack.Replicated))
    assertEquals(t.replicaStats.partitions.head.replicas(1).lag, 4L)

    rs(1).paused = false
    t.promote(0, replica = 1)
    // the successor was caught up by the promotion itself
    assertEquals(t.end(0), 4L)
    assertEquals(records(t.read(0, 0, 10)).map(r => str(r.value)),
      (0 until 4).map(i => s"v$i").toVector)
    assertEquals(t.append(0, bytes("k"), bytes("v4"), Ack.Replicated), 4L)
  }
