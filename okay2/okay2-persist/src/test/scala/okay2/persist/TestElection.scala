package okay2.persist

import munit.FunSuite

/** the battery over the memory control topic */
class TestElection extends ElectionSuite {
  def mkControl(): Topic = new MemoryStore().topic("__control")
}

/**
 * Election driving stage 2 (okay-persist's TestElectionReplicated): the
 * fold's winner calls the SAME promote the operator uses; nothing
 * acknowledged is lost across an automatic takeover; the deposed leader
 * is fenced; a dead arbiter degrades failover availability and NOTHING
 * else.
 */
class TestElectionReplicated extends FunSuite {

  // Live, as okay-persist tags it (flakes-integration, 2026-09-03): an
  // operator decision, not evidence against the suite — it binds no
  // port and starts no thread; its one failure was a suite-level runner
  // error under matrix load. `integrationTest` runs it.
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")
  private def str(b: Array[Byte]): String = new String(b, "UTF-8")

  var now = 0L

  /** three data replicas + a control topic; node names ARE replica
   * indices, so the fold's winner knows whom to promote */
  def cluster(): (Replicated, Topic, Vector[Election]) = {
    now = 0L
    val control = new MemoryStore().topic("__control")
    val data = Replicated("events", 1, Policy(), Vector.fill(3)(new MemoryStore: Store))
    val nodes = Vector("0", "1", "2").map(n => new Election(control, n, leaseMillis = 5000, skewMillis = 1000, clock = () => now))
    (data, control, nodes)
  }

  private def records(r: Topic.Read): Vector[Record] = r match {
    case Topic.Read.Records(rs) => rs
    case Topic.Read.TooEarly(b) => fail(s"unexpected TooEarly($b)")
  }

  test("a lease that stops renewing is taken over; nothing acknowledged is lost") {
    val (data, _, nodes) = cluster()
    val a = nodes(0)
    val b = nodes(1)
    // node 0 wins and leads; the coordinator starts at epoch 1 with
    // leader 0, so no promote is needed for the first winner
    assertEquals(a.tryTakeover(0), Some(1L))
    (0 until 4).foreach(i => data.append(0, bytes("k"), bytes(s"v$i"), Ack.Replicated))

    // node 0 goes quiet; its lease runs out
    now = 5000 + 1001
    assertEquals(b.tryTakeover(0), Some(2L))
    // the fold's winner drives the SAME promote the operator uses
    data.promote(0, replica = b.node.toInt)

    assertEquals(data.end(0), 4L)
    assertEquals(records(data.read(0, 0, 10)).map(r => str(r.value)), (0 until 4).map(i => s"v$i").toVector,
      "an acknowledged record vanished in the takeover")
    assertEquals(data.append(0, bytes("k"), bytes("v4"), Ack.Replicated), 4L)
  }

  test("the paused-and-resumed old leader is fenced on its first append; the rejection is an ops event") {
    val (data, _, nodes) = cluster()
    assertEquals(nodes(0).tryTakeover(0), Some(1L))
    val oldHandle = data.leader(0)     // what node 0 held before it paused

    now = 6001
    assertEquals(nodes(1).tryTakeover(0), Some(2L))
    data.promote(0, replica = 1)

    intercept[Replicated.Fenced](data.append(oldHandle, bytes("k"), bytes("late"), Ack.Durable))
    assertEquals(data.replicaStats.partitions.head.epoch, 2L)
  }

  test("arbiter down: data serves, failover waits, the operator path still works") {
    val (data, control, nodes) = cluster()
    assertEquals(nodes(0).tryTakeover(0), Some(1L))
    (0 until 2).foreach(i => data.append(0, bytes("k"), bytes(s"v$i"), Ack.Replicated))

    // the arbiter dies: every control append now throws
    val dead = new Topic {
      def name: String = control.name
      def partitions: Int = control.partitions
      def append(p: Int, k: Array[Byte], v: Array[Byte], ack: Ack): Long = throw new IllegalStateException("the arbiter is down")
      def read(p: Int, from: Long, max: Int): Topic.Read = control.read(p, from, max)
      def begin(p: Int): Long = control.begin(p)
      def end(p: Int): Long = control.end(p)
      def compact(p: Int): Unit = control.compact(p)
    }
    val bDark = new Election(dead, "1", 5000, 1000, () => now)

    now = 6001
    // failover WAITS: the claim cannot land
    intercept[IllegalStateException](bDark.tryTakeover(0))
    // data partitions keep serving under the standing leader
    assertEquals(data.append(0, bytes("k"), bytes("v2"), Ack.Replicated), 2L)
    // and the human is never locked out
    data.promote(0, replica = 1)
    assertEquals(data.end(0), 3L)
  }
}
