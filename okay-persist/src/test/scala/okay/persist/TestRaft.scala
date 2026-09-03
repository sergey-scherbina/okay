package okay.persist

/**
 * specs/consensus.md, persist-raft stage 0 — the algorithm's core
 * safety properties, driven EXPLICITLY (no wall clock, no
 * autonomous timers): a test calls `electionTimeout`/`deliverAll`
 * itself, the same manual-driving style TestElectionReplicated
 * already uses for the reduction's own battery. A richer
 * autonomous, seed-swept harness over Sim.scala (specs/sim.md) is
 * filed as the next slice, not reinvented here.
 */
class TestRaft extends munit.FunSuite {

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")
  private def text(e: RaftEntry): String = new String(e.data, "UTF-8")

  /** three nodes, an in-memory message bus; `deliverAll` runs to
   * quiescence — every message any handle() produces is delivered
   * before the call returns, so a test never races its own asserts */
  final class Cluster(ids: Vector[String] = Vector("0", "1", "2")) {
    var states: Map[String, RaftState] = ids.map(i => i -> RaftState(id = i)).toMap
    private val peersOf: Map[String, Set[String]] = ids.map(i => i -> (ids.toSet - i)).toMap
    private var inbox: Map[String, Vector[RaftMsg]] = ids.map(_ -> Vector.empty).toMap

    private def enqueue(out: Vector[RaftOut]): Unit =
      out.foreach(o => inbox = inbox.updated(o.to, inbox(o.to) :+ o.msg))

    def deliverAll(): Unit =
      var progressed = true
      while progressed do
        progressed = false
        for id <- ids do
          val pending = inbox(id)
          if pending.nonEmpty then
            inbox = inbox.updated(id, Vector.empty)
            for m <- pending do
              val (ns, out) = Raft.handle(states(id), m, peersOf(id))
              states = states.updated(id, ns)
              enqueue(out)
            progressed = true

    /** an election timeout fires on `id` */
    def electionTimeout(id: String): Unit =
      val (ns, out) = Raft.startElection(states(id), peersOf(id))
      states = states.updated(id, ns)
      enqueue(out)

    /** a leader's periodic heartbeat / replication tick */
    def heartbeat(id: String): Unit =
      enqueue(Raft.replicate(states(id), peersOf(id)))

    /** a client submits one entry to the (assumed) leader; queues
     * its replication at once, same as a real leader would */
    def clientAppend(leaderId: String, data: String): Unit =
      val s = states(leaderId)
      val ns = s.copy(log = s.log :+ RaftEntry(s.currentTerm, bytes(data)))
      states = states.updated(leaderId, ns)
      enqueue(Raft.replicate(ns, peersOf(leaderId)))

    def leaders: Set[String] = ids.filter(states(_).role == RaftRole.Leader).toSet
  }

  test("a candidate that times out first wins a majority and becomes leader") {
    val c = Cluster()
    c.electionTimeout("0")
    c.deliverAll()
    assertEquals(c.states("0").role, RaftRole.Leader)
    assertEquals(c.states("1").role, RaftRole.Follower)
    assertEquals(c.states("2").role, RaftRole.Follower)
    assertEquals(c.states("1").leaderId, Some("0"))
    assertEquals(c.states("2").leaderId, Some("0"))
    assertEquals(c.states("0").currentTerm, 1L)
  }

  test("election safety: two simultaneous candidates — exactly one wins the term, or the term is retried") {
    val c = Cluster()
    // both 0 and 1 time out before either hears from the other —
    // classic split-vote setup (each votes for itself first)
    c.electionTimeout("0")
    c.electionTimeout("1")
    c.deliverAll()
    val leadersAtHighestTerm = c.states.values.filter(_.role == RaftRole.Leader)
    // AT MOST one leader ever, at any term — Raft's central safety
    // property; a genuine split vote (no leader yet) is legal too,
    // and is resolved by a NEW, higher-term election
    assert(leadersAtHighestTerm.size <= 1, s"more than one leader: ${c.states}")
    if leadersAtHighestTerm.isEmpty then
      // split vote: nobody reached a majority at this term. A second
      // round (higher term) must succeed
      c.electionTimeout("2")
      c.deliverAll()
      assertEquals(c.leaders.size, 1, s"a retried election must converge: ${c.states}")
  }

  test("a client entry replicates to a majority and the LEADER commits it") {
    val c = Cluster()
    c.electionTimeout("0")
    c.deliverAll()
    c.clientAppend("0", "v0")
    c.deliverAll()
    assertEquals(c.states("0").commitIndex, 1L)
    assertEquals(c.states("0").log.map(text), Vector("v0"))
  }

  test("a heartbeat propagates the leader's commitIndex to followers") {
    val c = Cluster()
    c.electionTimeout("0")
    c.deliverAll()
    c.clientAppend("0", "v0")
    c.deliverAll()
    assertEquals(c.states("1").commitIndex, 0L, "not yet told")
    c.heartbeat("0")
    c.deliverAll()
    assertEquals(c.states("1").commitIndex, 1L)
    assertEquals(c.states("2").commitIndex, 1L)
  }

  test("log matching: a follower with a conflicting suffix is corrected, not merely appended to") {
    val c = Cluster()
    c.electionTimeout("0")
    c.deliverAll()
    c.clientAppend("0", "v0")
    c.clientAppend("0", "v1")
    c.deliverAll()
    assertEquals(c.states("1").log.map(text), Vector("v0", "v1"))

    // node 1 quietly diverges (as if it had accepted a stray entry
    // from a DIFFERENT term — an old, never-elected leader — the
    // only way two logs may legally disagree at one index is a
    // different TERM there, never the same term with different
    // content) — the NEXT AppendEntries from the real leader must
    // overwrite the bad suffix, not append past it
    val bad = c.states("1")
    c.states = c.states.updated("1",
      bad.copy(log = bad.log.updated(1, RaftEntry(bad.currentTerm - 1, bytes("ROGUE")))))
    c.clientAppend("0", "v2")
    c.deliverAll()
    assertEquals(c.states("1").log.map(text), Vector("v0", "v1", "v2"),
      "the follower's diverged entry must be overwritten by the leader's log")
  }

  test("a stale term is refused; a higher term steps a leader down") {
    val c = Cluster()
    c.electionTimeout("0")
    c.deliverAll()
    assertEquals(c.states("0").role, RaftRole.Leader)

    // a stale RequestVote (an old term) changes nothing — node 1
    // already voted for 0 in term 1 during the election above, and
    // a refused, lower-term message must not touch that
    val before = c.states("1").votedFor
    assertEquals(before, Some("0"))
    val (s1, out1) = Raft.handle(c.states("1"),
      RaftMsg.RequestVote(0, "2", 0, 0), Set("0", "2"))
    assertEquals(s1.votedFor, before, "a refused stale message must not touch existing state")
    assertEquals(out1, Vector(RaftOut("2", RaftMsg.RequestVoteResp(1, "1", false))))

    // a message naming a HIGHER term steps the standing leader down
    val (s0, _) = Raft.handle(c.states("0"),
      RaftMsg.AppendEntriesResp(99, "1", success = false, matchIndex = 0), Set("1", "2"))
    assertEquals(s0.role, RaftRole.Follower)
    assertEquals(s0.currentTerm, 99L)
  }

  test("commit safety: a majority match in an OLDER term is not committed by count alone") {
    // the textbook Figure 8 trap: a leader must not commit an entry
    // from a PREVIOUS term just because a majority now has it —
    // only counting a majority for an entry from its OWN term is
    // allowed to advance commitIndex
    val base = RaftState(id = "0", currentTerm = 2, role = RaftRole.Leader,
      log = Vector(RaftEntry(term = 1, data = bytes("old"))),
      matchIndex = Map("1" -> 1L, "2" -> 0L))
    val (ns, _) = Raft.handle(base,
      RaftMsg.AppendEntriesResp(2, "2", success = true, matchIndex = 1L), Set("1", "2"))
    assertEquals(ns.commitIndex, 0L,
      "an old-term entry must not commit by majority count alone (Figure 8)")
  }
}
