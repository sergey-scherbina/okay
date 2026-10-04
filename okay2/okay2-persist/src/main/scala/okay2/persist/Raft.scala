package okay2.persist

import okay2.codec.Schema

/**
 * Own Raft (okay-persist's Raft.scala; specs/consensus.md, persist-raft):
 * the consensus ALGORITHM's core state machine — leader election, log
 * replication, single-server membership changes (stage 2a), log
 * compaction with snapshots (stage 2b), pre-vote — as a pure value
 * transition: no engine, no network, no Store. The textbook core (Ongaro
 * & Ousterhout, Figure 2 and §7) plus Ongaro's thesis §4.1 and §4.2.3.
 * Not here: the catch-up (non-voting) phase for a joining server, chunked
 * snapshots.
 */

/** one log entry: the term it was appended under plus an opaque payload.
 * A non-empty `members` makes it a CONFIGURATION entry, in force from the
 * moment it is appended (thesis §4.1); empty `data` with no members is a
 * leader's blank no-op (paper §8), applied by nobody */
final case class RaftEntry(term: Long, data: Array[Byte], members: Vector[String] = Vector.empty)

object RaftEntry {
  implicit lazy val schema: Schema[RaftEntry] = Schema.derived
}

/** PreCandidate: asking, at its next term without adopting it, whether
 * the others would vote for it (thesis §4.2.3, §9.6) */
sealed trait RaftRole

object RaftRole {
  case object Follower extends RaftRole
  case object PreCandidate extends RaftRole
  case object Candidate extends RaftRole
  case object Leader extends RaftRole
}

/**
 * Everything one node knows: the persistent state the paper names
 * (currentTerm, votedFor, log), the volatile bookkeeping a
 * candidate/leader needs, `configIndex` (the latest configuration
 * entry's index, 0: the bootstrap peers are the cluster) and the
 * snapshot: `log` holds only the entries AFTER `snapshotIndex` — Raft's
 * index i is `log(i - snapshotIndex - 1)`. `restored` counts snapshots
 * installed from a leader: an engine that sees it change resets its
 * state machine to `snapshotData`.
 */
final case class RaftState(
  id: String,
  currentTerm: Long = 0,
  votedFor: Option[String] = None,
  log: Vector[RaftEntry] = Vector.empty,
  commitIndex: Long = 0,
  role: RaftRole = RaftRole.Follower,
  leaderId: Option[String] = None,
  votesGranted: Set[String] = Set.empty,
  nextIndex: Map[String, Long] = Map.empty,
  matchIndex: Map[String, Long] = Map.empty,
  configIndex: Long = 0,
  snapshotIndex: Long = 0,
  snapshotTerm: Long = 0,
  snapshotMembers: Vector[String] = Vector.empty,
  snapshotData: Array[Byte] = Array.empty,
  restored: Long = 0,
  preVotes: Set[String] = Set.empty)

sealed trait RaftMsg {
  /** the term the message carries; a PreVote's is hypothetical and
   * counts as 0 — it steps nobody up */
  def stepTerm: Long
}

object RaftMsg {
  final case class RequestVote(term: Long, candidateId: String, lastLogIndex: Long, lastLogTerm: Long) extends RaftMsg {
    def stepTerm: Long = term
  }
  final case class RequestVoteResp(term: Long, from: String, voteGranted: Boolean) extends RaftMsg {
    def stepTerm: Long = term
  }
  /** the pre-vote (thesis §4.2.3, §9.6): `term` is the term the asker
   * WOULD campaign at; a voter grants only if it would vote for that log
   * AND has not heard from a leader within an election timeout */
  final case class PreVote(term: Long, candidateId: String, lastLogIndex: Long, lastLogTerm: Long) extends RaftMsg {
    def stepTerm: Long = 0L
  }
  final case class PreVoteResp(term: Long, from: String, granted: Boolean) extends RaftMsg {
    def stepTerm: Long = term
  }
  final case class AppendEntries(term: Long, leaderId: String, prevLogIndex: Long, prevLogTerm: Long,
                                 entries: Vector[RaftEntry], leaderCommit: Long) extends RaftMsg {
    def stepTerm: Long = term
  }
  /** the follower's answer to AppendEntries AND to InstallSnapshot:
   * `matchIndex` is what the message ESTABLISHED on the follower */
  final case class AppendEntriesResp(term: Long, from: String, success: Boolean, matchIndex: Long) extends RaftMsg {
    def stepTerm: Long = term
  }
  /** a follower carrying a client's proposal to the leader: `n` is the
   * proposer's own sequence */
  final case class Propose(term: Long, from: String, n: Long, data: Array[Byte]) extends RaftMsg {
    def stepTerm: Long = term
  }
  /** the leader's answer to a Propose: accepted, or refused naming the
   * leader it knows of (empty when none) */
  final case class Proposed(term: Long, from: String, n: Long, accepted: Boolean, leader: String) extends RaftMsg {
    def stepTerm: Long = term
  }
  /** the leader's whole snapshot for a follower whose next entry it has
   * compacted away (paper §7, one message) */
  final case class InstallSnapshot(term: Long, leaderId: String, lastIndex: Long, lastTerm: Long,
                                   members: Vector[String], data: Array[Byte]) extends RaftMsg {
    def stepTerm: Long = term
  }

  implicit lazy val schema: Schema[RaftMsg] = Schema.derived
}

/** one outgoing message, addressed */
final case class RaftOut(to: String, msg: RaftMsg)

object Raft {

  def lastLogIndex(s: RaftState): Long = s.snapshotIndex + s.log.length
  def lastLogTerm(s: RaftState): Long = s.log.lastOption.map(_.term).getOrElse(s.snapshotTerm)
  private def majority(clusterSize: Int): Int = clusterSize / 2 + 1

  /** the entry at Raft index `i`, which must lie inside the log */
  private def entry(s: RaftState, i: Long): RaftEntry = s.log((i - s.snapshotIndex - 1).toInt)

  /** the term at Raft index `i`: the snapshot's own at its edge, none for
   * what was compacted away or lies past the end */
  def termAt(s: RaftState, i: Long): Option[Long] =
    if (i == s.snapshotIndex) Some(s.snapshotTerm)
    else if (i > s.snapshotIndex && i <= lastLogIndex(s)) Some(entry(s, i).term)
    else None

  /** the cluster this node counts majorities over: the latest
   * configuration entry in its log, else the snapshot's, else the
   * bootstrap `peers` (every OTHER node) plus itself */
  def members(s: RaftState, peers: Set[String]): Set[String] =
    if (s.configIndex > s.snapshotIndex) entry(s, s.configIndex).members.toSet
    else if (s.snapshotMembers.nonEmpty) s.snapshotMembers.toSet
    else peers + s.id

  /** everyone else in the current configuration */
  def others(s: RaftState, peers: Set[String]): Set[String] = members(s, peers) - s.id

  /** append one entry on a leader, keeping `configIndex` current */
  def append(s: RaftState, e: RaftEntry): RaftState = {
    val ns = s.copy(log = s.log :+ e)
    if (e.members.nonEmpty) ns.copy(configIndex = lastLogIndex(ns)) else ns
  }

  /** the configuration index once a follower's log has become `merged`:
   * its first `kept` entries retained, the rest the leader's. A truncated
   * configuration REVERTS to the one before it (thesis §4.1) */
  private def configAfter(s: RaftState, merged: Vector[RaftEntry], kept: Int): Long = {
    val inNew = merged.lastIndexWhere(_.members.nonEmpty)
    if (inNew >= kept) s.snapshotIndex + inNew + 1
    else if (s.configIndex <= s.snapshotIndex + kept) s.configIndex
    else {
      val inKept = merged.lastIndexWhere(_.members.nonEmpty, kept - 1)
      if (inKept >= 0) s.snapshotIndex + inKept + 1 else s.snapshotIndex
    }
  }

  /**
   * Log compaction (stage 2b, paper §7): the ENGINE has written its state
   * machine as of the committed index `upTo` into `snapshot`, and the log
   * up to there is dropped; the term and configuration in force at `upTo`
   * move into the snapshot fields. A no-op when `upTo` is not past the
   * current snapshot or not yet committed.
   */
  def compact(s: RaftState, upTo: Long, snapshot: Array[Byte]): RaftState =
    if (upTo <= s.snapshotIndex || upTo > s.commitIndex) s
    else {
      val term = termAt(s, upTo).getOrElse(s.snapshotTerm)
      val ms =
        if (s.configIndex > s.snapshotIndex && s.configIndex <= upTo) entry(s, s.configIndex).members
        else s.snapshotMembers
      s.copy(log = s.log.drop((upTo - s.snapshotIndex).toInt),
        snapshotIndex = upTo, snapshotTerm = term, snapshotMembers = ms, snapshotData = snapshot)
    }

  /** an election timeout fired: ask for PRE-votes at the next term
   * (thesis §4.2.3) — the term is untouched until a majority says it
   * would vote. A node the configuration does not name does not
   * campaign; a cluster of one skips the asking */
  def startElection(s: RaftState, peers: Set[String]): (RaftState, Vector[RaftOut]) =
    if (!members(s, peers)(s.id)) (s, Vector.empty)
    else {
      val os = others(s, peers)
      if (os.isEmpty) campaign(s, peers)
      else {
        val ns = s.copy(role = RaftRole.PreCandidate, preVotes = Set(s.id))
        (ns, os.toVector.map(p =>
          RaftOut(p, RaftMsg.PreVote(ns.currentTerm + 1, ns.id, lastLogIndex(ns), lastLogTerm(ns)))))
      }
    }

  /** the election proper: the next term, a vote for self, RequestVote to
   * every member */
  private def campaign(s: RaftState, peers: Set[String]): (RaftState, Vector[RaftOut]) = {
    val ns = s.copy(currentTerm = s.currentTerm + 1, votedFor = Some(s.id),
      role = RaftRole.Candidate, votesGranted = Set(s.id), leaderId = None, preVotes = Set.empty)
    (ns, others(ns, peers).toVector.map(p =>
      RaftOut(p, RaftMsg.RequestVote(ns.currentTerm, ns.id, lastLogIndex(ns), lastLogTerm(ns)))))
  }

  /** a leader's replication tick (also the heartbeat) — call after every
   * log change AND periodically */
  def replicate(s: RaftState, peers: Set[String]): Vector[RaftOut] = replicateTo(s, others(s, peers))

  private def replicateTo(s: RaftState, targets: Set[String]): Vector[RaftOut] =
    if (s.role != RaftRole.Leader) Vector.empty
    else targets.toVector.map { p =>
      val ni = s.nextIndex.getOrElse(p, lastLogIndex(s) + 1)
      if (ni <= s.snapshotIndex)
        // the follower's next entry is inside the snapshot: the whole snapshot
        RaftOut(p, RaftMsg.InstallSnapshot(s.currentTerm, s.id, s.snapshotIndex, s.snapshotTerm,
          s.snapshotMembers, s.snapshotData))
      else {
        val prevIdx = ni - 1
        val prevTerm = termAt(s, prevIdx).getOrElse(0L)
        val entries = s.log.drop((prevIdx - s.snapshotIndex).toInt)
        RaftOut(p, RaftMsg.AppendEntries(s.currentTerm, s.id, prevIdx, prevTerm, entries, s.commitIndex))
      }
    }

  /**
   * A membership change on the leader (stage 2a, thesis §4.1): the whole
   * new cluster as ONE configuration entry. One change at a time — `None`
   * while the previous configuration entry is not yet committed — and
   * `None` off the leader or for an empty cluster. `data` rides along for
   * the transport (the wire puts the members' addresses there).
   */
  def reconfigure(s: RaftState, peers: Set[String], newMembers: Set[String],
                  data: Array[Byte] = Array.empty): Option[(RaftState, Vector[RaftOut])] =
    if (s.role != RaftRole.Leader || s.configIndex > s.commitIndex || newMembers.isEmpty) None
    else {
      val ns = append(s, RaftEntry(s.currentTerm, data, newMembers.toVector.sorted))
      Some((ns, replicate(ns, peers)))
    }

  /** the ONE state transition: a message in, the new state plus whatever
   * it answers or forwards. `peers` is this node's BOOTSTRAP view of
   * everyone ELSE; `leaderFresh` is the caller's word that this node heard
   * from a leader within an election timeout — it decides a pre-vote and
   * nothing else */
  def handle(s0: RaftState, msg: RaftMsg, peers: Set[String], leaderFresh: Boolean = false): (RaftState, Vector[RaftOut]) = {
    // SEEING a higher term steps anyone down to a term-less follower,
    // before anything else
    val s =
      if (msg.stepTerm > s0.currentTerm)
        s0.copy(currentTerm = msg.stepTerm, votedFor = None, role = RaftRole.Follower, leaderId = None)
      else s0

    msg match {
      case m: RaftMsg.PreVote => preVote(s, m, leaderFresh)
      case m: RaftMsg.PreVoteResp => preVoteResp(s, m, peers)
      case m: RaftMsg.RequestVote => requestVote(s, m)
      case m: RaftMsg.RequestVoteResp => requestVoteResp(s, m, peers)
      case m: RaftMsg.AppendEntries => appendEntries(s, m)
      case m: RaftMsg.InstallSnapshot => installSnapshot(s, m)
      case m: RaftMsg.AppendEntriesResp => appendEntriesResp(s, m, peers)
      case m: RaftMsg.Propose => propose(s, m, peers)
      // the answer is for the node that forwarded: it changes no term,
      // no log, no vote
      case _: RaftMsg.Proposed => (s, Vector.empty)
    }
  }

  private def upToDate(s: RaftState, lastIdx: Long, lastTerm: Long): Boolean =
    lastTerm > lastLogTerm(s) || (lastTerm == lastLogTerm(s) && lastIdx >= lastLogIndex(s))

  private def preVote(s: RaftState, m: RaftMsg.PreVote, leaderFresh: Boolean): (RaftState, Vector[RaftOut]) = {
    // a leader never grants, nor a node that still hears its leader
    val granted = m.term >= s.currentTerm && upToDate(s, m.lastLogIndex, m.lastLogTerm) &&
      !leaderFresh && s.role != RaftRole.Leader
    (s, Vector(RaftOut(m.candidateId, RaftMsg.PreVoteResp(s.currentTerm, s.id, granted))))
  }

  private def preVoteResp(s: RaftState, m: RaftMsg.PreVoteResp, peers: Set[String]): (RaftState, Vector[RaftOut]) =
    if (s.role != RaftRole.PreCandidate || !m.granted) (s, Vector.empty)
    else {
      val ms = members(s, peers)
      val votes = (s.preVotes + m.from).filter(ms)
      if (votes.size < majority(ms.size)) (s.copy(preVotes = votes), Vector.empty)
      else campaign(s, peers)
    }

  private def requestVote(s: RaftState, m: RaftMsg.RequestVote): (RaftState, Vector[RaftOut]) = {
    val refuse = (s, Vector(RaftOut(m.candidateId, RaftMsg.RequestVoteResp(s.currentTerm, s.id, false))))
    if (m.term < s.currentTerm) refuse
    else if (s.votedFor.forall(_ == m.candidateId) && upToDate(s, m.lastLogIndex, m.lastLogTerm))
      (s.copy(votedFor = Some(m.candidateId)),
        Vector(RaftOut(m.candidateId, RaftMsg.RequestVoteResp(s.currentTerm, s.id, true))))
    else refuse
  }

  private def requestVoteResp(s: RaftState, m: RaftMsg.RequestVoteResp, peers: Set[String]): (RaftState, Vector[RaftOut]) =
    if (s.role != RaftRole.Candidate || m.term != s.currentTerm || !m.voteGranted) (s, Vector.empty)
    else {
      // only members' votes count
      val ms = members(s, peers)
      val votes = (s.votesGranted + m.from).filter(ms)
      if (votes.size < majority(ms.size)) (s.copy(votesGranted = votes), Vector.empty)
      else {
        // won: become leader, optimistic nextIndex, and append the
        // paper's blank no-op (§8) of ITS OWN term at once
        val os = ms - s.id
        val ni = os.map(_ -> (lastLogIndex(s) + 1)).toMap
        val leader = append(s.copy(role = RaftRole.Leader, votesGranted = votes,
          leaderId = Some(s.id), nextIndex = ni, matchIndex = os.map(_ -> 0L).toMap),
          RaftEntry(s.currentTerm, Array.empty))
        (leader, replicateTo(leader, os))
      }
    }

  private def appendEntries(s: RaftState, m: RaftMsg.AppendEntries): (RaftState, Vector[RaftOut]) =
    if (m.term < s.currentTerm)
      (s, Vector(RaftOut(m.leaderId, RaftMsg.AppendEntriesResp(s.currentTerm, s.id, false, 0))))
    else {
      // a valid leader for our term: acknowledge it
      val st = s.copy(role = RaftRole.Follower, leaderId = Some(m.leaderId))
      val prevIdx0 = m.prevLogIndex
      val entries0 = m.entries
      // a message reaching back into our snapshot agrees up to it (it is
      // committed): skip that much and go on from the snapshot's edge
      val (prevIdx, entries, logOk) =
        if (prevIdx0 < st.snapshotIndex) (st.snapshotIndex, entries0.drop((st.snapshotIndex - prevIdx0).toInt), true)
        else (prevIdx0, entries0, termAt(st, prevIdx0).contains(m.prevLogTerm))
      if (!logOk) (st, Vector(RaftOut(m.leaderId, RaftMsg.AppendEntriesResp(st.currentTerm, st.id, false, 0))))
      else {
        // splice in the entries, deleting only what CONFLICTS (paper
        // §5.3) — never a suffix that merely goes further than this
        // message: an older AppendEntries arriving late must not truncate
        // what the follower already acknowledged
        val at = (prevIdx - st.snapshotIndex).toInt   // log position of the first new entry
        var agree = 0
        while (agree < entries.length && at + agree < st.log.length && st.log(at + agree).term == entries(agree).term)
          agree += 1
        val merged =
          if (agree == entries.length) st.log
          else st.log.take(at + agree) ++ entries.drop(agree)
        // what THIS message establishes — not the log's length
        val established = math.max(prevIdx0 + entries0.length, st.snapshotIndex)
        val newCommit =
          if (m.leaderCommit > st.commitIndex) math.max(st.commitIndex, math.min(m.leaderCommit, established))
          else st.commitIndex
        val nst = st.copy(log = merged, commitIndex = newCommit, configIndex = configAfter(st, merged, at + agree))
        (nst, Vector(RaftOut(m.leaderId, RaftMsg.AppendEntriesResp(nst.currentTerm, nst.id, true, established))))
      }
    }

  private def installSnapshot(s: RaftState, m: RaftMsg.InstallSnapshot): (RaftState, Vector[RaftOut]) =
    if (m.term < s.currentTerm)
      (s, Vector(RaftOut(m.leaderId, RaftMsg.AppendEntriesResp(s.currentTerm, s.id, false, 0))))
    else {
      val st = s.copy(role = RaftRole.Follower, leaderId = Some(m.leaderId))
      if (m.lastIndex <= st.snapshotIndex)
        // nothing we do not already have: say how far we are
        (st, Vector(RaftOut(m.leaderId, RaftMsg.AppendEntriesResp(st.currentTerm, st.id, true, st.snapshotIndex))))
      else {
        // paper §7: keep the suffix that follows an entry agreeing with
        // the snapshot's last one; otherwise the snapshot is the whole log
        val keep =
          if (termAt(st, m.lastIndex).contains(m.lastTerm)) st.log.drop((m.lastIndex - st.snapshotIndex).toInt)
          else Vector.empty
        val nst = st.copy(log = keep,
          snapshotIndex = m.lastIndex, snapshotTerm = m.lastTerm, snapshotMembers = m.members, snapshotData = m.data,
          commitIndex = math.max(st.commitIndex, m.lastIndex),
          configIndex = if (keep.nonEmpty && st.configIndex > m.lastIndex) st.configIndex else m.lastIndex,
          restored = st.restored + 1)
        (nst, Vector(RaftOut(m.leaderId, RaftMsg.AppendEntriesResp(nst.currentTerm, nst.id, true, m.lastIndex))))
      }
    }

  private def appendEntriesResp(s: RaftState, m: RaftMsg.AppendEntriesResp, peers: Set[String]): (RaftState, Vector[RaftOut]) =
    if (s.role != RaftRole.Leader || m.term != s.currentTerm) (s, Vector.empty)
    else if (!m.success) {
      // log-matching backoff: retry one index earlier
      val ni = math.max(1L, s.nextIndex.getOrElse(m.from, 1L) - 1)
      val nst = s.copy(nextIndex = s.nextIndex.updated(m.from, ni))
      (nst, replicateTo(nst, Set(m.from)))
    } else {
      val nst = s.copy(
        matchIndex = s.matchIndex.updated(m.from, m.matchIndex),
        nextIndex = s.nextIndex.updated(m.from, m.matchIndex + 1))
      // commit safety (Raft §5.4.2): the highest N a MAJORITY of the
      // CURRENT configuration has matched, but ONLY when log(N) is of
      // the CURRENT term
      val ms = members(nst, peers)
      val matched = ms.toVector.map(x =>
        if (x == nst.id) lastLogIndex(nst) else nst.matchIndex.getOrElse(x, 0L)).sorted
      val n = matched(matched.length - majority(ms.size))
      val committed =
        if (n > nst.commitIndex && n >= 1 && termAt(nst, n).contains(nst.currentTerm)) n
        else nst.commitIndex
      val out = nst.copy(commitIndex = committed)
      // a leader whose own removal just committed steps down — after one
      // last heartbeat carrying that commit
      if (committed > nst.commitIndex && committed >= out.configIndex && !ms(out.id))
        (out.copy(role = RaftRole.Follower, leaderId = None), replicateTo(out, ms))
      else (out, Vector.empty)
    }

  private def propose(s: RaftState, m: RaftMsg.Propose, peers: Set[String]): (RaftState, Vector[RaftOut]) =
    // the leader appends a forwarded proposal as its OWN entry and
    // replicates at once; anyone else refuses naming the leader it knows
    if (s.role == RaftRole.Leader) {
      val nst = append(s, RaftEntry(s.currentTerm, m.data))
      (nst, RaftOut(m.from, RaftMsg.Proposed(nst.currentTerm, nst.id, m.n, true, nst.id)) +: replicate(nst, peers))
    } else (s, Vector(RaftOut(m.from, RaftMsg.Proposed(s.currentTerm, s.id, m.n, false, s.leaderId.getOrElse("")))))
}
