package okay2.persist

import scala.collection.mutable.ArrayBuffer

/**
 * specs/consensus.md, persist-raft stage 1a (okay-persist's
 * TestRaftWire) — REAL sockets, REAL threads, REAL wall-clock timing:
 * the algorithm survives an actual network, not just an in-process bus.
 *
 * `Live`, as okay-persist tags it (the operator's rule, 2026-09-07: real
 * sockets, threads and wall-clock waits failed three full gates in two
 * days under matrix load while passing in isolation); `integrationTest`
 * runs it, where it must still be green.
 */
class TestRaftWire extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(90, "s")
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  private def freePort(): Int = {
    val s = new java.net.ServerSocket(0)
    try s.getLocalPort finally s.close()
  }

  /** THE BUDGETS ARE THE BOX'S, NOT THE PROTOCOL'S: the nodes keep their
   * timings (tick 20ms, election 200-400ms, heartbeat 50ms); the waits
   * around them are what a loaded box can meet */
  private val Elect = 15000L
  private val Settle = 6000L
  private val Commit = 15000L

  /** `freePort` closes its socket and the node binds later; three tries
   * make that window a non-event, a half-built cluster closed between */
  private def cluster(tries: Int = 3): Cluster =
    try new Cluster
    catch {
      case _: java.net.BindException if tries > 1 =>
        Thread.sleep(50)
        cluster(tries - 1)
    }

  /** three real nodes on localhost, each with its own thread pair and a
   * commit log the test can inspect */
  final class Cluster {
    val ids = Vector("0", "1", "2")
    val ports: Map[String, Int] = ids.map(_ -> freePort()).toMap
    val addr: Map[String, (String, Int)] = ports.map { case (id, p) => id -> (("127.0.0.1", p)) }
    val commits: Map[String, ArrayBuffer[(Long, String)]] = ids.map(_ -> ArrayBuffer.empty[(Long, String)]).toMap
    val nodes: Map[String, RaftWire.Node] = {
      val built = ArrayBuffer.empty[(String, RaftWire.Node)]
      try {
        for (id <- ids)
          built += id -> new RaftWire.Node(id, ports(id), addr - id,
            tickMs = 20, electionTimeoutMs = 200, heartbeatMs = 50,
            onCommit = (i, e) => if (e.members.isEmpty) commits(id).synchronized { commits(id) += (i -> new String(e.data, "UTF-8")); () })
        built.toMap
      } catch {
        case e: Throwable =>
          // a later node's bind failed: the earlier ones must not leak
          built.foreach(_._2.close())
          throw e
      }
    }

    def leader: Option[String] = ids.find(id => nodes(id).isLeader)

    def close(): Unit = nodes.values.foreach(_.close())
  }

  private def waitUntil(timeoutMs: Long)(cond: => Boolean): Boolean = {
    val deadline = System.currentTimeMillis() + timeoutMs
    while (!cond && System.currentTimeMillis() < deadline) Thread.sleep(10)
    cond
  }

  test("three real nodes over real sockets elect exactly one leader") {
    val c = cluster()
    try {
      assert(waitUntil(Elect)(c.leader.isDefined), s"no leader elected: ${c.ids.map(c.nodes(_).currentTerm)}")
      assertEquals(c.ids.count(id => c.nodes(id).isLeader), 1, "exactly one node must be leader")
      // read the leader INSIDE the wait: under load a heartbeat can be
      // missed and a new term elected while the others are learning the
      // old one, and `leader.get` between the two would read None
      assert(waitUntil(Settle)(c.leader.exists(l => c.ids.forall(id => c.nodes(id).leaderId.contains(l)))),
        s"nodes disagree on the leader: ${c.ids.map(id => id -> c.nodes(id).leaderId)}")
    } finally c.close()
  }

  test("a client entry proposed to the leader replicates and commits on every node") {
    val c = cluster()
    try {
      assert(waitUntil(Elect)(c.leader.isDefined))
      val leader = c.leader.get
      assert(c.nodes(leader).propose("hello-raft".getBytes("UTF-8")))
      // a non-leader that knows its leader CARRIES the proposal there
      val follower = c.ids.find(_ != leader).get
      assert(c.nodes(follower).propose("via-follower".getBytes("UTF-8")))

      // index 1 is the term's blank no-op (paper §8)
      assert(waitUntil(Commit)(c.ids.forall(id => c.nodes(id).commitIndex >= 3)),
        s"not every node committed both: ${c.ids.map(id => id -> c.nodes(id).commitIndex)}")
      c.ids.foreach { id =>
        assert(waitUntil(Settle)(c.commits(id).synchronized(c.commits(id).count(_._2.nonEmpty)) >= 2), s"node $id never got both onCommits")
        assertEquals(c.commits(id).synchronized(c.commits(id).toVector).filter(_._2.nonEmpty).map(_._2).take(2),
          Vector("hello-raft", "via-follower"))
      }
    } finally c.close()
  }

  test("killing the leader: the survivors elect a new one and keep committing") {
    val c = cluster()
    try {
      assert(waitUntil(Elect)(c.leader.isDefined))
      val firstLeader = c.leader.get
      assert(c.nodes(firstLeader).propose("before-kill".getBytes("UTF-8")))
      assert(waitUntil(Commit)(c.ids.forall(id => c.nodes(id).commitIndex >= 2)))

      c.nodes(firstLeader).close()
      val survivors = c.ids.filter(_ != firstLeader)

      assert(waitUntil(Elect)(survivors.exists(id => c.nodes(id).isLeader)),
        s"no survivor took over: ${survivors.map(id => id -> c.nodes(id).currentTerm)}")
      val newLeader = survivors.find(id => c.nodes(id).isLeader).get
      assert(newLeader != firstLeader)

      assert(c.nodes(newLeader).propose("after-kill".getBytes("UTF-8")))
      // no-op, before-kill, the new term's no-op, after-kill
      assert(waitUntil(Commit)(survivors.forall(id => c.nodes(id).commitIndex >= 4)),
        s"the survivors did not commit after failover: ${survivors.map(id => id -> c.nodes(id).commitIndex)}")
    } finally c.close()
  }

  test("membership over the wire: a fourth node joins and is counted; the leader removes itself and the rest go on") {
    val c = cluster()
    val p3 = freePort()
    val joined = ArrayBuffer.empty[(Long, String)]
    // the newcomer is started knowing the cluster; it is nobody's member
    // until the leader says so
    val n3 = new RaftWire.Node("3", p3, c.addr, tickMs = 20, electionTimeoutMs = 200, heartbeatMs = 50,
      onCommit = (i, e) => if (e.members.isEmpty) joined.synchronized { joined += (i -> new String(e.data, "UTF-8")); () })
    val all = c.addr + ("3" -> (("127.0.0.1", p3)))
    try {
      assert(waitUntil(Elect)(c.leader.isDefined))
      val leader = c.leader.get
      assert(c.nodes(leader).propose("before-join".getBytes("UTF-8")))
      assert(waitUntil(Commit)(c.nodes(leader).commitIndex >= 2))   // the no-op, then before-join

      assert(c.nodes(leader).reconfigure(all), "the leader accepts a change with none pending")
      assert(waitUntil(Commit)(n3.commitIndex >= 3 && n3.members == all.keySet),
        s"the newcomer did not learn the cluster: commit ${n3.commitIndex}, members ${n3.members}")
      assert(waitUntil(Settle)(c.ids.forall(id => c.nodes(id).members == all.keySet)),
        s"the old members disagree on the cluster: ${c.ids.map(id => id -> c.nodes(id).members)}")
      assert(c.nodes(leader).propose("after-join".getBytes("UTF-8")))
      assert(waitUntil(Commit)(n3.commitIndex >= 4), "the newcomer commits like any member")
      assertEquals(joined.synchronized(joined.toVector).filter(_._2.nonEmpty).map(_._2),
        Vector("before-join", "after-join"), "neither the no-op nor the configuration entry is applied")

      // the leader removes itself: leads until the change commits, then steps down
      assert(c.nodes(leader).reconfigure(all - leader))
      val rest = (c.ids :+ "3").filter(_ != leader)
      val node = (id: String) => if (id == "3") n3 else c.nodes(id)
      assert(waitUntil(Elect)(!c.nodes(leader).isLeader && rest.exists(id => node(id).isLeader)),
        s"no successor among $rest: ${rest.map(id => id -> node(id).currentTerm)}")
      val successor = rest.find(id => node(id).isLeader).get
      assertEquals(node(successor).members, (all - leader).keySet)
      assert(node(successor).propose("after-remove".getBytes("UTF-8")))
      // ... the removal entry, the successor's no-op, after-remove
      assert(waitUntil(Commit)(rest.forall(id => node(id).commitIndex >= 7)),
        s"the rest did not commit after the removal: ${rest.map(id => id -> node(id).commitIndex)}")
      // the removed node never campaigns
      val termAtRemoval = node(successor).currentTerm
      Thread.sleep(600)
      assert(c.nodes(leader).currentTerm <= termAtRemoval, "a removed server must stay silent")
      assert(!c.nodes(leader).isLeader)
    } finally {
      n3.close(); c.close()
    }
  }

  test("compaction over the wire: a node that starts late is restored from the leader's snapshot and goes on") {
    val ids = Vector("0", "1", "2")
    val ports = ids.map(_ -> freePort()).toMap
    val addr = ports.map { case (id, p) => id -> (("127.0.0.1", p)) }
    val applied = ids.map(_ -> ArrayBuffer.empty[String]).toMap
    val restored = ArrayBuffer.empty[(Long, String)]
    def node(id: String) = new RaftWire.Node(id, ports(id), addr - id, tickMs = 20, electionTimeoutMs = 200, heartbeatMs = 50,
      onCommit = (_, e) => if (e.data.nonEmpty && e.members.isEmpty) applied(id).synchronized { applied(id) += new String(e.data, "UTF-8"); () },
      onRestore = (at, bytes) => restored.synchronized { restored += (at -> new String(bytes, "UTF-8")); () })
    val early = Vector("0", "1").map(id => id -> node(id)).toMap
    var late: Option[RaftWire.Node] = None
    try {
      assert(waitUntil(Elect)(early.values.exists(_.isLeader)))
      val leader = early.find(_._2.isLeader).get._2
      for (v <- Seq("a", "b", "c")) assert(leader.propose(v.getBytes("UTF-8")))
      assert(waitUntil(Commit)(early.values.forall(_.commitIndex >= 4)))   // no-op + a, b, c
      // the engine's snapshot of "the state machine as of index 4"
      assert(leader.compact(4, "a,b,c".getBytes("UTF-8")))
      assertEquals(leader.snapshotIndex, 4L)
      assert(leader.propose("d".getBytes("UTF-8")))
      assert(waitUntil(Commit)(leader.commitIndex >= 5))

      val n2 = node("2")
      late = Some(n2)
      assert(waitUntil(Commit)(n2.snapshotIndex == 4 && n2.commitIndex >= 5),
        s"the late node was not restored: snapshot ${n2.snapshotIndex}, commit ${n2.commitIndex}")
      assertEquals(restored.synchronized(restored.toVector), Vector((4L, "a,b,c")))
      // what it applied itself: only what came after the snapshot
      assert(waitUntil(Settle)(applied("2").synchronized(applied("2").toVector) == Vector("d")),
        s"applied on the late node: ${applied("2")}")
      assert(leader.propose("e".getBytes("UTF-8")))
      assert(waitUntil(Commit)(applied("2").synchronized(applied("2").toVector) == Vector("d", "e")))
    } finally {
      late.foreach(_.close()); early.values.foreach(_.close())
    }
  }
}
