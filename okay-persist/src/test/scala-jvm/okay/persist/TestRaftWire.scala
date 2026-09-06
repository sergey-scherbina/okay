package okay.persist

/**
 * specs/consensus.md, persist-raft stage 1a — REAL sockets, REAL
 * threads, REAL wall-clock timing (not the explicit-tick harness
 * TestRaft uses for the pure algorithm core): proves the algorithm
 * survives an actual network, not just an in-process message bus.
 */
class TestRaftWire extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(90, "s")

  /** INTEGRATION, not the default gate (operator's rule, 2026-09-07:
   * every flake moves to the integration tests). Real sockets, real
   * threads and wall-clock waits made this suite fail three full gates
   * in two days under matrix load while passing 3/3 in isolation every
   * time; it runs under `sbt integrationTest`, where it still must be
   * green -- the budgets and the retrying cluster below are for that
   * run, not an excuse for it. */
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  private def freePort(): Int =
    val s = new java.net.ServerSocket(0)
    try s.getLocalPort finally s.close()

  /**
   * THE BUDGETS ARE THE BOX'S, NOT THE PROTOCOL'S. The nodes keep their
   * timings (tick 20ms, election 200-400ms, heartbeat 50ms) -- that is
   * the law under test. The waits around them are what a loaded matrix
   * can meet: this suite failed three full gates in two days at load
   * 25-30 ("the survivors did not commit after failover" against a
   * 3000ms wait) and was green 3/3 in isolation every time
   * (raft-wire-election-flake). A wait that returns early costs a
   * fast box nothing; a wait that is too short costs everyone a gate.
   */
  private val Elect = 15000L
  private val Settle = 6000L
  private val Commit = 15000L

  /**
   * `freePort` closes its socket and the node binds later, and between
   * the two any process on the box can take the port -- the
   * BindException that failed a gate on 2026-09-06. Three tries make
   * that window a non-event; a cluster that failed to bind is closed
   * before the next attempt, so no half-built node leaks a thread.
   */
  private def cluster(tries: Int = 3): Cluster =
    try Cluster()
    catch case _: java.net.BindException if tries > 1 =>
      Thread.sleep(50)
      cluster(tries - 1)

  /** three real nodes on localhost, each with its own thread pair
   * (accept loop + tick loop) and a commit log the test can inspect */
  final class Cluster {
    val ids = Vector("0", "1", "2")
    val ports = ids.map(_ -> freePort()).toMap
    val addr = ports.map((id, p) => id -> ("127.0.0.1", p))
    val commits = ids.map(_ -> collection.mutable.ArrayBuffer.empty[(Long, String)]).toMap
    val nodes: Map[String, RaftWire.Node] =
      val built = collection.mutable.ArrayBuffer.empty[(String, RaftWire.Node)]
      try
        for id <- ids do
          built += id -> RaftWire.Node(id, ports(id), addr - id,
            tickMs = 20, electionTimeoutMs = 200, heartbeatMs = 50,
            onCommit = (i, e) => commits(id).synchronized {
              commits(id) += (i -> new String(e.data, "UTF-8")): Unit
            })
        built.toMap
      catch case e: Throwable =>
        // a later node's bind failed: the earlier ones must not leak
        // their accept and tick threads into the next attempt
        built.foreach(_._2.close())
        throw e

    def leader: Option[String] = ids.find(id => nodes(id).isLeader)

    def close(): Unit = nodes.values.foreach(_.close())
  }

  private def waitUntil(timeoutMs: Long)(cond: => Boolean): Boolean =
    val deadline = System.currentTimeMillis() + timeoutMs
    while !cond && System.currentTimeMillis() < deadline do Thread.sleep(10)
    cond

  test("three real nodes over real sockets elect exactly one leader") {
    val c = cluster()
    try
      assert(waitUntil(Elect)(c.leader.isDefined), s"no leader elected: ${c.ids.map(c.nodes(_).currentTerm)}")
      val leaders = c.ids.count(id => c.nodes(id).isLeader)
      assertEquals(leaders, 1, "exactly one node must be leader")
      // every OTHER node must recognize the SAME leader (once
      // propagated — a heartbeat cycle or two)
      assert(waitUntil(Settle)(c.ids.forall(id => c.nodes(id).leaderId.contains(c.leader.get))),
        s"nodes disagree on the leader: ${c.ids.map(id => id -> c.nodes(id).leaderId)}")
    finally c.close()
  }

  test("a client entry proposed to the leader replicates and commits on every node") {
    val c = cluster()
    try
      assert(waitUntil(Elect)(c.leader.isDefined))
      val leader = c.leader.get
      assert(c.nodes(leader).propose("hello-raft".getBytes("UTF-8")))
      // a non-leader refuses
      val follower = c.ids.find(_ != leader).get
      assert(!c.nodes(follower).propose("nope".getBytes("UTF-8")))

      assert(waitUntil(Commit)(c.ids.forall(id => c.nodes(id).commitIndex >= 1)),
        s"not every node committed: ${c.ids.map(id => id -> c.nodes(id).commitIndex)}")
      c.ids.foreach { id =>
        assert(waitUntil(Settle)(c.commits(id).nonEmpty), s"node $id never got onCommit")
        assertEquals(c.commits(id).head, (1L, "hello-raft"))
      }
    finally c.close()
  }

  test("killing the leader: the survivors elect a new one and keep committing") {
    val c = cluster()
    try
      assert(waitUntil(Elect)(c.leader.isDefined))
      val firstLeader = c.leader.get
      assert(c.nodes(firstLeader).propose("before-kill".getBytes("UTF-8")))
      assert(waitUntil(Commit)(c.ids.forall(id => c.nodes(id).commitIndex >= 1)))

      c.nodes(firstLeader).close()
      val survivors = c.ids.filter(_ != firstLeader)

      assert(waitUntil(Elect)(survivors.exists(id => c.nodes(id).isLeader)),
        s"no survivor took over: ${survivors.map(id => id -> c.nodes(id).currentTerm)}")
      val newLeader = survivors.find(id => c.nodes(id).isLeader).get
      assert(newLeader != firstLeader)

      assert(c.nodes(newLeader).propose("after-kill".getBytes("UTF-8")))
      assert(waitUntil(Commit)(survivors.forall(id => c.nodes(id).commitIndex >= 2)),
        s"the survivors did not commit after failover: ${survivors.map(id => id -> c.nodes(id).commitIndex)}")
    finally c.close()
  }
}
