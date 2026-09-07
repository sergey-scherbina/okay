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
    /** nodes the network cannot reach right now: messages to them are lost */
    var down: Set[String] = Set.empty

    /** the engine's side of compaction: this node has applied up to
     * `upTo` and snapshotted its state machine as `snapshot` */
    def compact(id: String, upTo: Long, snapshot: String): Boolean =
      val ns = Raft.compact(states(id), upTo, bytes(snapshot))
      val did = ns.snapshotIndex != states(id).snapshotIndex
      states = states.updated(id, ns)
      did

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
      out.foreach(o => if !down(o.to) then inbox = inbox.updated(o.to, inbox(o.to) :+ o.msg))

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

  // ---- stage 2b: compaction and InstallSnapshot (paper §7) ---------

  test("compaction drops the log up to an applied index and keeps its term, its configuration and the engine's bytes") {
    val c = Cluster()
    c.electionTimeout("0"); c.deliverAll()
    c.clientAppend("0", "v0"); c.clientAppend("0", "v1"); c.deliverAll()
    c.addNode("3")
    assert(c.reconfigure("0", Set("0", "1", "2", "3"))); c.deliverAll()
    c.clientAppend("0", "v2"); c.deliverAll()
    // log: no-op, v0, v1, config, v2 — commitIndex 5
    assertEquals(c.states("0").commitIndex, 5L)
    assert(!c.compact("0", 6, "S"), "not committed: refused")
    assert(c.compact("0", 4, "S4"), "up to the configuration entry")
    val s = c.states("0")
    assertEquals(s.snapshotIndex, 4L)
    assertEquals(s.snapshotTerm, 1L)
    assertEquals(s.log.map(text), Vector("v2"))
    assertEquals(Raft.lastLogIndex(s), 5L)
    assertEquals(Raft.members(s, Set("1", "2")), Set("0", "1", "2", "3"), "the configuration lives on in the snapshot")
    assertEquals(new String(s.snapshotData, "UTF-8"), "S4")
    assert(!c.compact("0", 3, "older"), "not past the current snapshot: refused")
    // the leader goes on committing over the compacted log
    c.clientAppend("0", "v3"); c.deliverAll()
    assertEquals(c.states("0").commitIndex, 6L)
    assertEquals(c.states("1").log.map(text), Vector("", "v0", "v1", "", "v2", "v3"))
  }

  test("a follower that missed a compacted stretch is sent the snapshot, restores, and goes on from its edge") {
    val c = Cluster()
    c.electionTimeout("0"); c.deliverAll()
    c.clientAppend("0", "v0"); c.deliverAll()
    c.heartbeat("0"); c.deliverAll()
    assertEquals(c.states("2").commitIndex, 2L)
    // node 2 drops off; the majority commits v1..v3 and the leader compacts through v2
    c.down = Set("2")
    for v <- Seq("v1", "v2", "v3") do { c.clientAppend("0", v); c.deliverAll() }
    assertEquals(c.states("0").commitIndex, 5L)
    assert(c.compact("0", 4, "machine@4"))
    c.clientAppend("0", "v4"); c.deliverAll()
    // node 2 is back: the leader's nextIndex for it (3, from before it
    // dropped off) is inside the snapshot, so the next heartbeat is an
    // InstallSnapshot; the one after carries the entries past its edge
    c.down = Set.empty
    c.heartbeat("0"); c.deliverAll()
    assertEquals(c.states("2").restored, 1L, "one snapshot installed")
    assertEquals(c.states("2").log, Vector.empty, "the log after the snapshot comes with the next replication")
    c.heartbeat("0"); c.deliverAll()
    val s2 = c.states("2")
    assertEquals(s2.restored, 1L, "one snapshot installed")
    assertEquals(s2.snapshotIndex, 4L)
    assertEquals(new String(s2.snapshotData, "UTF-8"), "machine@4")
    assertEquals(s2.log.map(text), Vector("v3", "v4"), "the log after the snapshot came by ordinary replication")
    assertEquals(s2.commitIndex, c.states("0").commitIndex)
    assertEquals(c.states("0").matchIndex("2"), 6L)
    // and it keeps following: a later entry commits on it like on any member
    c.clientAppend("0", "v5"); c.deliverAll(); c.heartbeat("0"); c.deliverAll()
    assertEquals(c.states("2").commitIndex, 7L)
    assertEquals(c.states("2").log.map(text), Vector("v3", "v4", "v5"))
  }

  test("a snapshot no newer than the follower's own changes nothing; a follower keeps the suffix its snapshot agrees with") {
    // a follower already past that snapshot: nothing to install, it reports where it is
    val ahead = RaftState(id = "1", currentTerm = 2, snapshotIndex = 5, snapshotTerm = 2, snapshotData = bytes("F5"), commitIndex = 5)
    val (a, outA) = Raft.handle(ahead, RaftMsg.InstallSnapshot(2, "0", 3, 1, Vector.empty, bytes("L3")), Set("0", "2"))
    assertEquals(a.restored, 0L)
    assertEquals(a.snapshotIndex, 5L)
    assertEquals(outA, Vector(RaftOut("0", RaftMsg.AppendEntriesResp(2, "1", true, 5))))
    // a follower whose log holds the snapshot's last entry keeps what follows it
    val partial = RaftState(id = "2", currentTerm = 2,
      log = Vector(RaftEntry(1, Array.empty), RaftEntry(1, bytes("v0")), RaftEntry(1, bytes("v1")), RaftEntry(1, bytes("v2"))),
      commitIndex = 2)
    val (p, outP) = Raft.handle(partial, RaftMsg.InstallSnapshot(2, "0", 3, 1, Vector.empty, bytes("L3")), Set("0", "1"))
    assertEquals(p.restored, 1L)
    assertEquals(p.snapshotIndex, 3L)
    assertEquals(p.log.map(text), Vector("v2"), "the suffix after the snapshot's edge survives")
    assertEquals(p.commitIndex, 3L)
    assertEquals(outP, Vector(RaftOut("0", RaftMsg.AppendEntriesResp(2, "2", true, 3))))
    // a follower whose entry at the edge has another term drops everything
    val conflicting = partial.copy(log = partial.log.updated(2, RaftEntry(0, bytes("stray"))))
    val (q, _) = Raft.handle(conflicting, RaftMsg.InstallSnapshot(2, "0", 3, 1, Vector.empty, bytes("L3")), Set("0", "1"))
    assertEquals(q.log, Vector.empty)
    assertEquals(Raft.lastLogIndex(q), 3L)
    assertEquals(Raft.lastLogTerm(q), 1L)
  }
}
