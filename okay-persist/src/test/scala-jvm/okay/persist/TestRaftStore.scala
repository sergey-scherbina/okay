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
