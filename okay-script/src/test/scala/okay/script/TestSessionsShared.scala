package okay.script

import okay.persist.{MemoryStore, Policy, RemoteStore, Replicated, Wire}

/** okay-script-cluster-sessions: `Sessions.shared` over a replicated
 * okay-persist topic -- a session set on one node is read on another.
 * See specs/okay-script.md "Clustered sessions". The wire test binds
 * a port and is Live-tagged.
 */
class TestSessionsShared extends munit.FunSuite:

  private val poll = java.time.Duration.ofMillis(20)

  private def eventually(deadlineMs: Long = 5000)(cond: => Boolean): Boolean =
    val end = System.currentTimeMillis() + deadlineMs
    var ok = cond
    while !ok && System.currentTimeMillis() < end do
      Thread.sleep(10)
      ok = cond
    ok

  private def replicated(): Replicated =
    Replicated("__sessions", 1, Policy(compact = true), Vector.fill(3)(MemoryStore()))

  test("in-process: two nodes over one Replicated topic -- set on A is read on B; invalidate on B is gone on A") {
    val coord = replicated()
    val a = Sessions.shared(coord, poll = poll)
    val b = Sessions.shared(coord, poll = poll)
    try
      val ha = a.handle(None)
      ha.set("cart", "ok-1")
      val id = ha.id
      assert(eventually()(b.handle(Some(id)).get("cart").contains("ok-1")), "B never saw A's write")
      assertEquals(b.size, 1)

      b.handle(Some(id)).set("cart", "ok-1,ok-2")
      assert(eventually()(a.handle(Some(id)).get("cart").contains("ok-1,ok-2")), "A never saw B's update")

      b.handle(Some(id)).invalidate()
      assert(eventually()(a.handle(Some(id)).get("cart").isEmpty), "A never saw B's invalidate")
      assert(eventually()(a.size == 0 && b.size == 0))
    finally
      a.close()
      b.close()

  }

  test("a node's own rapid writes never regress: after the tailer passes, A still reads its latest value") {
    val coord = replicated()
    val a = Sessions.shared(coord, poll = poll)
    try
      val h = a.handle(None)
      for i <- 1 to 50 do h.set("n", i.toString)
      val id = h.id
      Thread.sleep(poll.toMillis * 5)
      assertEquals(a.handle(Some(id)).get("n"), Some("50"))
      // the skip set drains: nothing of ours is left for the tailer
      assert(eventually()(coord.end(0) == 51L))
    finally a.close()
  }

  test("over the wire: the coordinator behind Wire.Server, a second node on RemoteStore -- both directions".tag(new munit.Tag("Live"))) {
    val coord = replicated()
    val srv = Wire.Server(MemoryStore(), { case "node" => Some(Set(coord.name)); case _ => None },
      repl = name => Option.when(name == coord.name)(coord))
    val leader = Sessions.shared(coord, poll = poll)
    val remote = Wire.Remote.connect("127.0.0.1", srv.port, "node")
    val follower = Sessions.shared(RemoteStore(remote).topic(coord.name), poll = poll)
    try
      val hf = follower.handle(None)
      hf.set("cart", "ok-3")
      val id = hf.id
      assert(eventually()(leader.handle(Some(id)).get("cart").contains("ok-3")), "the coordinator node never saw the remote node's write")

      leader.handle(Some(id)).set("cart", "ok-3,ok-4")
      assert(eventually()(follower.handle(Some(id)).get("cart").contains("ok-3,ok-4")), "the remote node never saw the coordinator's update")
    finally
      follower.close()
      leader.close()
      remote.close()
      srv.close()
  }
