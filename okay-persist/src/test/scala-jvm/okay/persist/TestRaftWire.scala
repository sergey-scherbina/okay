package okay.persist

/**
 * specs/consensus.md, persist-raft stage 1a — REAL sockets, REAL
 * threads, REAL wall-clock timing (not the explicit-tick harness
 * TestRaft uses for the pure algorithm core): proves the algorithm
 * survives an actual network, not just an in-process message bus.
 */
class TestRaftWire extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(30, "s")

  private def freePort(): Int =
    val s = new java.net.ServerSocket(0)
    try s.getLocalPort finally s.close()

  /** three real nodes on localhost, each with its own thread pair
   * (accept loop + tick loop) and a commit log the test can inspect */
  final class Cluster {
    val ids = Vector("0", "1", "2")
    val ports = ids.map(_ -> freePort()).toMap
    val addr = ports.map((id, p) => id -> ("127.0.0.1", p))
    val commits = ids.map(_ -> collection.mutable.ArrayBuffer.empty[(Long, String)]).toMap
    val nodes = ids.map { id =>
      id -> RaftWire.Node(id, ports(id), addr - id,
        tickMs = 20, electionTimeoutMs = 200, heartbeatMs = 50,
        onCommit = (i, e) => commits(id).synchronized {
          commits(id) += (i -> new String(e.data, "UTF-8")): Unit
        })
    }.toMap

    def leader: Option[String] = ids.find(id => nodes(id).isLeader)

    def close(): Unit = nodes.values.foreach(_.close())
  }

  private def waitUntil(timeoutMs: Long)(cond: => Boolean): Boolean =
    val deadline = System.currentTimeMillis() + timeoutMs
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(10)
    cond

  test("three real nodes over real sockets elect exactly one leader") {
    val c = Cluster()
    try
      assert(waitUntil(5000)(c.leader.isDefined), s"no leader elected: ${c.ids.map(c.nodes(_).currentTerm)}")
      val leaders = c.ids.count(id => c.nodes(id).isLeader)
      assertEquals(leaders, 1, "exactly one node must be leader")
      // every OTHER node must recognize the SAME leader (once
      // propagated — a heartbeat cycle or two)
      assert(waitUntil(2000)(c.ids.forall(id => c.nodes(id).leaderId.contains(c.leader.get))),
        s"nodes disagree on the leader: ${c.ids.map(id => id -> c.nodes(id).leaderId)}")
    finally c.close()
  }

  test("a client entry proposed to the leader replicates and commits on every node") {
    val c = Cluster()
    try
      assert(waitUntil(5000)(c.leader.isDefined))
      val leader = c.leader.get
      assert(c.nodes(leader).propose("hello-raft".getBytes("UTF-8")))
      // a non-leader refuses
      val follower = c.ids.find(_ != leader).get
      assert(!c.nodes(follower).propose("nope".getBytes("UTF-8")))

      assert(waitUntil(5000)(c.ids.forall(id => c.nodes(id).commitIndex >= 1)),
        s"not every node committed: ${c.ids.map(id => id -> c.nodes(id).commitIndex)}")
      c.ids.foreach { id =>
        assert(waitUntil(2000)(c.commits(id).nonEmpty), s"node $id never got onCommit")
        assertEquals(c.commits(id).head, (1L, "hello-raft"))
      }
    finally c.close()
  }

  test("killing the leader: the survivors elect a new one and keep committing") {
    val c = Cluster()
    try
      assert(waitUntil(5000)(c.leader.isDefined))
      val firstLeader = c.leader.get
      assert(c.nodes(firstLeader).propose("before-kill".getBytes("UTF-8")))
      assert(waitUntil(3000)(c.ids.forall(id => c.nodes(id).commitIndex >= 1)))

      c.nodes(firstLeader).close()
      val survivors = c.ids.filter(_ != firstLeader)

      assert(waitUntil(5000)(survivors.exists(id => c.nodes(id).isLeader)),
        s"no survivor took over: ${survivors.map(id => id -> c.nodes(id).currentTerm)}")
      val newLeader = survivors.find(id => c.nodes(id).isLeader).get
      assert(newLeader != firstLeader)

      assert(c.nodes(newLeader).propose("after-kill".getBytes("UTF-8")))
      assert(waitUntil(3000)(survivors.forall(id => c.nodes(id).commitIndex >= 2)),
        s"the survivors did not commit after failover: ${survivors.map(id => id -> c.nodes(id).commitIndex)}")
    finally c.close()
  }
}
