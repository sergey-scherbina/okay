package okay.persist

/**
 * Stage 1b over real sockets (specs/consensus.md): three `RaftStore`s,
 * an append on the leader applied on every node, a follower's append
 * refused by name, a killed leader failed over and the survivors
 * still agreeing. `Live`, like `TestRaftWire` and for the same
 * reason: real ports and real threads flake on a loaded box, and the
 * gate should not depend on that.
 */
class TestRaftStore extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(90, "s")
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  private val Elect = 15000L
  private val Commit = 15000L

  private def freePort(): Int =
    val s = new java.net.ServerSocket(0)
    try s.getLocalPort finally s.close()

  private def await(what: String, ms: Long)(cond: => Boolean): Unit =
    val deadline = System.currentTimeMillis() + ms
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(20)
    assert(cond, s"$what did not happen within ${ms}ms")

  final class Cluster {
    val ids = Vector("0", "1", "2")
    val ports = ids.map(_ -> freePort()).toMap
    val addr = ports.map((id, p) => id -> ("127.0.0.1", p))
    val locals = ids.map(_ -> new MemoryStore).toMap
    val stores: Map[String, RaftStore] =
      ids.map(id => id -> RaftStore.start(id, ports(id), addr - id, locals(id), tickMs = 20,
        electionTimeoutMs = 150, heartbeatMs = 50)).toMap
    def leader: Option[String] = stores.values.find(_.isLeader).map(_.id)
    def close(): Unit = stores.values.foreach(_.close())
  }

  private def cluster(tries: Int = 3): Cluster =
    try new Cluster
    catch case _: java.net.BindException if tries > 1 =>
      Thread.sleep(50)
      cluster(tries - 1)

  private def valuesIn(local: Store, topic: String): Vector[String] =
    local.topic(topic).read(0, 0L, 100) match
      case Topic.Read.Records(rs) => rs.map(r => String(r.value, "UTF-8"))
      case Topic.Read.TooEarly(_) => Vector.empty

  private def recordsIn(local: Store, topic: String): Vector[(Long, String)] =
    local.topic(topic).read(0, 0L, 100) match
      case Topic.Read.Records(rs) => rs.map(r => (r.offset, String(r.value, "UTF-8")))
      case Topic.Read.TooEarly(_) => Vector.empty

  test("the store's snapshot: two nodes commit and compact, a third starts late and reads the same records at the same offsets") {
    val ids = Vector("0", "1", "2")
    val ports = ids.map(_ -> freePort()).toMap
    val addr = ports.map((id, p) => id -> ("127.0.0.1", p))
    val locals = ids.map(_ -> new MemoryStore).toMap
    def start(id: String) = RaftStore.start(id, ports(id), addr - id, locals(id),
      tickMs = 20, electionTimeoutMs = 150, heartbeatMs = 50)
    val early = Vector("0", "1").map(id => id -> start(id)).toMap
    var late: Option[RaftStore] = None
    try
      await("a leader", Elect)(early.values.exists(_.isLeader))
      val leader = early.values.find(_.isLeader).get
      val t = leader.topic("t", 1)
      for i <- 0 until 6 do assertEquals(t.append(0, Array.empty, s"v$i".getBytes("UTF-8"), Ack.Durable), i.toLong)
      await("both applied", Commit)(early.keys.forall(id => valuesIn(locals(id), "t").length == 6))
      assertEquals(leader.snapshot(), Right(leader.applied), "the image is taken at the last applied index")
      assertEquals(leader.snapshotIndex, leader.applied)
      // the log up to there is gone on the leader; the cluster goes on
      for i <- 6 until 8 do assertEquals(t.append(0, Array.empty, s"v$i".getBytes("UTF-8"), Ack.Durable), i.toLong)

      val third = start("2"); late = Some(third)
      await("the late node restored and caught up", Commit)(recordsIn(locals("2"), "t").length == 8)
      assertEquals(recordsIn(locals("2"), "t"), (0 until 8).map(i => (i.toLong, s"v$i")).toVector,
        "the same records at the same offsets: the image's, then the log's")
      assertEquals(third.damaged, None)
      assert(third.applied >= leader.snapshotIndex)
      assertEquals(third.topics, Vector("t"), "the topic was declared by the restore")
      // and it keeps applying — and a follower's append is carried to the leader as before
      assertEquals(third.topic("t", 1).append(0, Array.empty, "v8".getBytes("UTF-8"), Ack.Durable), 8L)
      await("the leader applied the late node's append", Commit)(valuesIn(locals(leader.id), "t").length == 9)
    finally
      late.foreach(_.close()); early.values.foreach(_.close())
  }

  test("the store's snapshot is refused once retention has dropped history: offsets could not survive a restore") {
    val c = cluster()
    try
      await("a leader", Elect)(c.leader.isDefined)
      val leader = c.stores(c.leader.get)
      // a topic that keeps a few hundred bytes: `begin` moves past 0
      val t = leader.topic("short", 1, Policy(retainBytes = 200))
      for i <- 0 until 20 do { val _ = t.append(0, Array.empty, s"value-number-$i".getBytes("UTF-8"), Ack.Durable) }
      await("applied", Commit)(leader.applied >= 20)
      assert(t.begin(0) > 0, s"retention did not move begin: ${t.begin(0)}")
      val refused = leader.snapshot()
      assert(refused.isLeft, s"a snapshot over dropped history must be refused: $refused")
      assert(refused.swap.exists(_.contains("begins at")), refused.toString)
      assertEquals(leader.snapshotIndex, 0L, "nothing compacted")
    finally c.close()
  }

  test("an append on the leader is applied on every node, and a follower's append names the leader") {
    val c = cluster()
    try
      await("a leader", Elect)(c.leader.isDefined)
      val leader = c.leader.get
      val t = c.stores(leader).topic("orders")
      val off = t.append("k1".getBytes, "first".getBytes)
      assertEquals(off, 0L)
      val off2 = t.append("k2".getBytes, "second".getBytes)
      assertEquals(off2, 1L)
      c.ids.foreach { id =>
        c.stores(id).topic("orders"): Unit   // declared on every node, as configuration is
        await(s"node $id applied both", Commit)(valuesIn(c.locals(id), "orders") == Vector("first", "second"))
      }
      // reads through the store are the local store's: the same on every node
      c.ids.foreach(id => assertEquals(c.stores(id).topic("orders").end(0), 2L))
      // a follower's append is CARRIED to the leader (persist-raft-forward):
      // it answers the offset its own node applied the entry at, and
      // every node has it
      val follower = c.ids.find(_ != leader).get
      val off3 = c.stores(follower).topic("orders").append("k3".getBytes, "third".getBytes)
      assertEquals(off3, 2L)
      c.ids.foreach(id => await(s"node $id applied the forwarded one", Commit)(
        valuesIn(c.locals(id), "orders") == Vector("first", "second", "third")))
    finally c.close()
  }

  test("the leader dies: the survivors elect, accept an append, and both apply it") {
    val c = cluster()
    try
      await("a leader", Elect)(c.leader.isDefined)
      val first = c.leader.get
      c.stores(first).topic("log").append("a".getBytes, "before".getBytes): Unit
      c.stores(first).close()
      val survivors = c.ids.filter(_ != first)
      await("a new leader among the survivors", Elect)(survivors.exists(id => c.stores(id).isLeader))
      val second = survivors.find(id => c.stores(id).isLeader).get
      val off = c.stores(second).topic("log").append("b".getBytes, "after".getBytes)
      assertEquals(off, 1L)
      survivors.foreach { id =>
        c.stores(id).topic("log"): Unit
        await(s"survivor $id applied both", Commit)(valuesIn(c.locals(id), "log") == Vector("before", "after"))
      }
    finally c.close()
  }
}
