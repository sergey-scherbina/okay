package okay.persist

import okay.{!, Async}
import okay.given
import munit.FunSuite

/**
 * The replication surface over the wire (specs/persist.md,
 * persist-wire-repl): the coordinator's calls (produce, promote) and
 * compact join the message enum, and — the point — the replication
 * MACHINERY does not change when a replica goes remote. Two directions
 * are proven: a remote CLIENT driving a Replicated hosted on a node,
 * and a Replicated holding a RemoteStore as one of its replicas.
 */
class TestWireRepl extends FunSuite:

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")
  private def str(b: Array[Byte]): String = new String(b, "UTF-8")
  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  /** a server that hosts a Replicated coordinator under the name it
   * was built for; the auth plug grants that topic */
  def coordServer(coord: Replicated): Wire.Server =
    Wire.Server(MemoryStore(),
      { case "op" => Some(Set(coord.name)); case _ => None },
      repl = name => Option.when(name == coord.name)(coord))

  def replicated(name: String, replicas: Int): Replicated =
    Replicated(name, 1, Policy(), Vector.fill(replicas)(MemoryStore()))

  test("produce over the wire is idempotent: the retry lands once, same offset") {
    val coord = replicated("orders", 3)
    val srv = coordServer(coord)
    try
      val c = Wire.Remote.connect("127.0.0.1", srv.port, "op")
      try
        val o1 = run(c.produce("orders", 0, "p1", 1L, bytes("k"), bytes("v1")))
        val o2 = run(c.produce("orders", 0, "p1", 1L, bytes("k"), bytes("v1"))) // the retry
        assertEquals(o1, 0L)
        assertEquals(o2, o1)                          // dropped, original offset
        assertEquals(coord.end(0), 1L)                // one record, not two
      finally c.close()
    finally srv.close()
  }

  test("promote over the wire is the operator's failover; a stale seq refuses beyond the window") {
    val coord = replicated("orders", 3)
    val srv = coordServer(coord)
    try
      val c = Wire.Remote.connect("127.0.0.1", srv.port, "op")
      try
        run(c.produce("orders", 0, "p1", 1L, Array.empty, bytes("a"))): Unit
        run(c.produce("orders", 0, "p1", 2L, Array.empty, bytes("b"))): Unit
        assertEquals(coord.replicaStats.partitions(0).leader, 0)

        run(c.promote("orders", 0, 2))
        val st = coord.replicaStats.partitions(0)
        assertEquals(st.leader, 2)
        assert(st.epoch > 1L, s"epoch ${st.epoch}")

        // the coordinator survived the failover: a fresh produce lands
        val o = run(c.produce("orders", 0, "p1", 3L, Array.empty, bytes("c")))
        assertEquals(o, 2L)

        // a replay from beyond the dedup window refuses by name
        val e = intercept[Wire.WireRefused](
          run(c.produce("orders", 0, "p1", 1L, Array.empty, bytes("old"))))
        assert(e.reason.contains("beyond the dedup window"), e.reason)
      finally c.close()
    finally srv.close()
  }

  test("produce/promote on a name that is not a replicated topic refuse by name") {
    // a plain store server: the name exists, but no coordinator backs it
    val srv = Wire.Server(MemoryStore(), { case "op" => Some(Set("plain")); case _ => None })
    try
      val c = Wire.Remote.connect("127.0.0.1", srv.port, "op")
      try
        val e1 = intercept[Wire.WireRefused](
          run(c.produce("plain", 0, "p1", 1L, Array.empty, bytes("x"))))
        assert(e1.reason.contains("not a replicated topic"), e1.reason)
        val e2 = intercept[Wire.WireRefused](run(c.promote("plain", 0, 1)))
        assert(e2.reason.contains("not a replicated topic"), e2.reason)
        // the connection survives; a plain append still works
        assertEquals(run(c.append("plain", 0, Array.empty, bytes("ok"))), 0L)
      finally c.close()
    finally srv.close()
  }

  test("compact completes the Topic surface over the wire: the sequence grows holes, end holds") {
    val store = MemoryStore()
    val srv = Wire.Server(store, { case "op" => Some(Set("kv")); case _ => None })
    try
      val c = Wire.Remote.connect("127.0.0.1", srv.port, "op")
      try
        run(c.append("kv", 0, bytes("k"), bytes("v0"))): Unit
        run(c.append("kv", 0, bytes("k"), bytes("v1"))): Unit
        val endBefore = run(c.end("kv", 0))
        run(c.compact("kv", 0))
        assertEquals(run(c.end("kv", 0)), endBefore)   // end does not move
        // the latest per key survives; the superseded record is gone
        run(c.read("kv", 0, 0L, 10)) match
          case Topic.Read.Records(rs) =>
            assertEquals(rs.map(r => str(r.value)), Vector("v1"))
          case other => fail(s"unexpected $other")
      finally c.close()
    finally srv.close()
  }

  test("a RemoteStore is a replica: Replicated drives a remote node, machinery unchanged") {
    // the far node: a plain store reachable over the wire
    val backing = MemoryStore()
    val srv = Wire.Server(backing, { case "op" => Some(Set("orders")); case _ => None })
    try
      val remote = Wire.Remote.connect("127.0.0.1", srv.port, "op")
      try
        val here = MemoryStore()
        val coord = Replicated("orders", 1, Policy(), Vector(here, RemoteStore(remote)))

        // an append through the leader eagerly pushes to the remote replica
        val off = coord.append(coord.leader(0), bytes("k"), bytes("hello"), Ack.Replicated)
        assertEquals(off, 0L)

        // the far node holds the very bytes we appended — no JVM replica
        // in this process wrote them, the wire did
        backing.topic("orders").read(0, 0L, 10) match
          case Topic.Read.Records(rs) => assertEquals(str(rs.head.value), "hello")
          case other => fail(s"unexpected $other")

        // the hwm reached quorum (both copies), so the record is readable
        assertEquals(coord.end(0), 1L)

        // now make the remote lag, then replicate-PULL catches it up
        here.topic("orders").append(0, bytes("k"), bytes("later"), Ack.Durable): Unit
        assert(backing.topic("orders").end(0) < here.topic("orders").end(0))
        coord.replicate(0)
        assertEquals(backing.topic("orders").end(0), 2L)
        backing.topic("orders").read(0, 1L, 10) match
          case Topic.Read.Records(rs) => assertEquals(str(rs.head.value), "later")
          case other => fail(s"unexpected $other")
      finally remote.close()
    finally srv.close()
  }
