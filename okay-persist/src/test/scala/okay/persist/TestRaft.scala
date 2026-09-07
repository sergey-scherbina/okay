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
  final class Cluster(var ids: Vector[String] = Vector("0", "1", "2")) {
    var states: Map[String, RaftState] = ids.map(i => i -> RaftState(id = i)).toMap
    /** each node's BOOTSTRAP view of the others; a configuration
     * entry in its log overrides it (stage 2a) */
    private var peersOf: Map[String, Set[String]] = ids.map(i => i -> (ids.toSet - i)).toMap
    private var inbox: Map[String, Vector[RaftMsg]] = ids.map(_ -> Vector.empty).toMap

    /** a fresh server, started knowing the current cluster (as an
     * operator would configure it), not yet a member of anything */
    def addNode(id: String): Unit =
      peersOf = peersOf.updated(id, ids.toSet)
      ids = ids :+ id
      states = states.updated(id, RaftState(id = id))
      inbox = inbox.updated(id, Vector.empty)

    /** an operator asks the leader for a new cluster; false when refused */
    def reconfigure(leaderId: String, members: Set[String]): Boolean =
      Raft.reconfigure(states(leaderId), peersOf(leaderId), members) match
        case None => false
        case Some((ns, out)) => states = states.updated(leaderId, ns); enqueue(out); true

    def members(id: String): Set[String] = Raft.members(states(id), peersOf(id))

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
      val ns = Raft.append(states(leaderId), RaftEntry(states(leaderId).currentTerm, bytes(data)))
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
    // a term opens with the leader's blank no-op (paper §8), so the
    // client's entry is the term's second
    assertEquals(c.states("0").log.map(text), Vector(""))
    c.clientAppend("0", "v0")
    c.deliverAll()
    assertEquals(c.states("0").commitIndex, 2L)
    assertEquals(c.states("0").log.map(text), Vector("", "v0"))
  }

  test("a heartbeat propagates the leader's commitIndex to followers") {
    val c = Cluster()
    c.electionTimeout("0")
    c.deliverAll()
    c.clientAppend("0", "v0")
    c.deliverAll()
    assertEquals(c.states("1").commitIndex, 1L, "the no-op's commit rode along with v0; v0's own not yet told")
    c.heartbeat("0")
    c.deliverAll()
    assertEquals(c.states("1").commitIndex, 2L)
    assertEquals(c.states("2").commitIndex, 2L)
  }

  test("log matching: a follower with a conflicting suffix is corrected, not merely appended to") {
    val c = Cluster()
    c.electionTimeout("0")
    c.deliverAll()
    c.clientAppend("0", "v0")
    c.clientAppend("0", "v1")
    c.deliverAll()
    assertEquals(c.states("1").log.map(text), Vector("", "v0", "v1"))

    // node 1 quietly diverges (as if it had accepted a stray entry
    // from a DIFFERENT term — an old, never-elected leader — the
    // only way two logs may legally disagree at one index is a
    // different TERM there, never the same term with different
    // content) — the NEXT AppendEntries from the real leader must
    // overwrite the bad suffix, not append past it
    val bad = c.states("1")
    c.states = c.states.updated("1",
      bad.copy(log = bad.log.updated(2, RaftEntry(bad.currentTerm - 1, bytes("ROGUE")))))
    c.clientAppend("0", "v2")
    c.deliverAll()
    assertEquals(c.states("1").log.map(text), Vector("", "v0", "v1", "v2"),
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

  // ---- stage 2a: membership changes (thesis §4.1) ------------------

  test("a server joins: the configuration entry replicates, the newcomer receives the log, and majorities count it") {
    val c = Cluster()
    c.electionTimeout("0"); c.deliverAll()
    c.clientAppend("0", "v0"); c.deliverAll()
    c.addNode("3")
    assert(c.reconfigure("0", Set("0", "1", "2", "3")))
    // in force on the leader at once, before anyone has heard of it
    assertEquals(c.members("0"), Set("0", "1", "2", "3"))
    c.deliverAll()
    for id <- c.ids do assertEquals(c.members(id), Set("0", "1", "2", "3"), s"node $id")
    assertEquals(c.states("3").log.map(text), Vector("", "v0", ""), "the newcomer was brought up to date")
    assertEquals(c.states("0").commitIndex, 3L, "the configuration entry committed")
    // a later entry needs 3 of 4 now — and the newcomer is one of them
    c.clientAppend("0", "v1"); c.deliverAll()
    assertEquals(c.states("0").commitIndex, 4L)
    c.heartbeat("0"); c.deliverAll()
    assertEquals(c.states("3").commitIndex, 4L)
  }

  test("one change at a time: a second change is refused while the first is uncommitted, accepted once it commits") {
    val c = Cluster()
    c.electionTimeout("0"); c.deliverAll()
    c.addNode("3"); c.addNode("4")
    assert(c.reconfigure("0", Set("0", "1", "2", "3")))
    assert(!c.reconfigure("0", Set("0", "1", "2", "3", "4")), "refused: the first change is not yet committed")
    assert(!c.reconfigure("1", Set("0", "1", "2", "3", "4")), "refused: not the leader")
    c.deliverAll()
    assert(c.reconfigure("0", Set("0", "1", "2", "3", "4")), "accepted once the first change committed")
    c.deliverAll()
    for id <- c.ids do assertEquals(c.members(id), Set("0", "1", "2", "3", "4"), s"node $id")
  }

  test("a leader removing itself leads until the change commits, tells the followers, then steps down; the rest elect") {
    val c = Cluster()
    c.electionTimeout("0"); c.deliverAll()
    c.clientAppend("0", "v0"); c.deliverAll()
    assert(c.reconfigure("0", Set("1", "2")))
    c.deliverAll()
    assertEquals(c.states("0").role, RaftRole.Follower, "stepped down once its removal committed")
    assertEquals(c.states("0").commitIndex, 3L)
    assertEquals(c.states("1").commitIndex, 3L, "the last heartbeat carried the commit")
    assertEquals(c.states("2").commitIndex, 3L)
    // the removed node does not campaign; the remaining two elect among themselves
    c.electionTimeout("0"); c.deliverAll()
    assertEquals(c.leaders, Set.empty, "a removed server stays silent")
    c.electionTimeout("1"); c.deliverAll()
    assertEquals(c.leaders, Set("1"))
    assertEquals(c.members("1"), Set("1", "2"))
    assertEquals(c.states("1").commitIndex, 4L, "the new term's no-op committed by 2 of 2")
    c.clientAppend("1", "v1"); c.deliverAll()
    assertEquals(c.states("1").commitIndex, 5L, "2 of 2 is the new majority")
  }

  test("a truncated configuration reverts: an uncommitted change from a deposed leader is undone with its entry") {
    val follower = RaftState(id = "1", currentTerm = 1,
      log = Vector(RaftEntry(1, bytes("v0")), RaftEntry(1, Array.empty, Vector("0", "1", "2", "3"))),
      configIndex = 2)
    assertEquals(Raft.members(follower, Set("0", "2")), Set("0", "1", "2", "3"))
    // a new leader (term 2) whose log has a different entry at index 2
    val (ns, _) = Raft.handle(follower,
      RaftMsg.AppendEntries(2, "2", 1, 1, Vector(RaftEntry(2, bytes("v1"))), 0), Set("0", "2"))
    assertEquals(ns.log.map(text), Vector("v0", "v1"))
    assertEquals(ns.configIndex, 0L)
    assertEquals(Raft.members(ns, Set("0", "2")), Set("0", "1", "2"), "back to the bootstrap cluster")
  }
}
