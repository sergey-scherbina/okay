package okay.persist

import okay.codec.Schema

/**
 * Own Raft (specs/consensus.md, persist-raft): the consensus
 * ALGORITHM's core state machine — leader election, log
 * replication, cluster membership changes (stage 2a) and log
 * compaction with snapshots (stage 2b) — safety proven by
 * simulation — as a pure value transition, no engine, no network,
 * no Store. `Election` (specs/consensus.md, the reduction) does not
 * change when this lands as a `RaftStore` behind a `Topic`: the
 * control log's total order is all Election ever asked an engine
 * for, and a Raft-replicated log is one more way to produce it.
 *
 * Deliberately NOT here (filed under the same BACKLOG name): the
 * catch-up (non-voting) phase for a joining server, pre-vote against
 * a removed server's disruption, chunked snapshots. This is the
 * textbook core (Ongaro & Ousterhout, Figure 2 and §7) plus the
 * single-server configuration change of Ongaro's thesis §4.1 —
 * proven here so the engine wrapper has an honest foundation.
 */

/** one log entry: the term it was appended under (the log matching
 * property needs it) plus an opaque payload — what the payload
 * MEANS is the Store engine's business, not this core's. A
 * non-empty `members` makes it a CONFIGURATION entry: the whole
 * cluster (the leader included) from this index on, in force from
 * the moment it is appended, committed or not (thesis §4.1); its
 * `data` is then the transport's (addresses, on the wire) and no
 * state machine applies it. Empty `data` with no members is a
 * leader's blank no-op (paper §8), the first entry of every term:
 * applied by nobody either. */
final case class RaftEntry(term: Long, data: Array[Byte], members: Vector[String] = Vector.empty) derives Schema

enum RaftRole:
  case Follower, Candidate, Leader

/**
 * Everything one node knows: the persistent state the paper names
 * (currentTerm, votedFor, log) plus the volatile bookkeeping a
 * candidate/leader needs (votesGranted, nextIndex, matchIndex),
 * `configIndex` — the index of the latest configuration entry (0:
 * none yet, the bootstrap peers are the cluster), kept current by
 * `Raft.append` and by AppendEntries' splice, so the cluster a
 * majority is counted over is always the log's own latest word —
 * and the snapshot (stage 2b): `log` holds only the entries AFTER
 * `snapshotIndex`, whose term, configuration and the ENGINE's
 * state-machine bytes the snapshot fields keep. `log` is 1-indexed
 * conceptually — Raft's index i is `log(i - snapshotIndex - 1)` —
 * so the paper's proofs read across unchanged. `restored` counts
 * snapshots installed from a leader: an engine that sees it change
 * resets its state machine to `snapshotData` at `snapshotIndex`.
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
  restored: Long = 0)

enum RaftMsg derives Schema:
  case RequestVote(term: Long, candidateId: String, lastLogIndex: Long, lastLogTerm: Long)
  case RequestVoteResp(term: Long, from: String, voteGranted: Boolean)
  case AppendEntries(term: Long, leaderId: String, prevLogIndex: Long, prevLogTerm: Long,
                     entries: Vector[RaftEntry], leaderCommit: Long)
  /** the follower's answer to AppendEntries AND to InstallSnapshot:
   * `matchIndex` is what the message ESTABLISHED on the follower */
  case AppendEntriesResp(term: Long, from: String, success: Boolean, matchIndex: Long)
  /** a follower carrying a client's proposal to the leader
   * (persist-raft-forward): `n` is the proposer's own sequence, so it
   * recognises the entry when it commits on ITS node */
  case Propose(term: Long, from: String, n: Long, data: Array[Byte])
  /** the leader's answer to a Propose: accepted and appended, or
   * refused with the leader it knows of (empty when none) */
  case Proposed(term: Long, from: String, n: Long, accepted: Boolean, leader: String)
  /** the leader's whole snapshot for a follower whose next entry it
   * has compacted away (paper §7, one message — not chunked yet):
   * the index and term it ends at, the configuration in force
   * there, and the engine's state-machine bytes */
  case InstallSnapshot(term: Long, leaderId: String, lastIndex: Long, lastTerm: Long,
                       members: Vector[String], data: Array[Byte])

/** one outgoing message, addressed */
final case class RaftOut(to: String, msg: RaftMsg)

object Raft:

  def lastLogIndex(s: RaftState): Long = s.snapshotIndex + s.log.length
  def lastLogTerm(s: RaftState): Long = s.log.lastOption.map(_.term).getOrElse(s.snapshotTerm)
  private def majority(clusterSize: Int): Int = clusterSize / 2 + 1

  /** the entry at Raft index `i`, which must lie inside the log */
  private def entry(s: RaftState, i: Long): RaftEntry = s.log((i - s.snapshotIndex - 1).toInt)

  /** the term at Raft index `i`: the snapshot's own at its edge (and
   * 0 at index 0 before any), none for what was compacted away or
   * lies past the end */
  def termAt(s: RaftState, i: Long): Option[Long] =
    if i == s.snapshotIndex then Some(s.snapshotTerm)
    else if i > s.snapshotIndex && i <= lastLogIndex(s) then Some(entry(s, i).term)
    else None

  /** the cluster this node counts majorities over: the latest
   * configuration entry in its log — committed or not, in force
   * from the moment it is appended (thesis §4.1) — else the one the
   * snapshot carries, else the bootstrap `peers` plus itself.
   * `peers` is every OTHER node this one was started knowing, never
   * itself. */
  def members(s: RaftState, peers: Set[String]): Set[String] =
    if s.configIndex > s.snapshotIndex then entry(s, s.configIndex).members.toSet
    else if s.snapshotMembers.nonEmpty then s.snapshotMembers.toSet
    else peers + s.id

  /** everyone else in the current configuration */
  def others(s: RaftState, peers: Set[String]): Set[String] = members(s, peers) - s.id

  /** append one entry on a leader, keeping `configIndex` current —
   * the ONE way a log grows outside AppendEntries */
  def append(s: RaftState, e: RaftEntry): RaftState =
    val ns = s.copy(log = s.log :+ e)
    if e.members.nonEmpty then ns.copy(configIndex = lastLogIndex(ns)) else ns

  /** the configuration index once a follower's log has become
   * `merged`: its first `kept` entries (log positions, after the
   * snapshot) retained, the rest the leader's. A truncated
   * configuration REVERTS to the one before it (thesis §4.1: a
   * server uses the latest configuration in its log, whatever that
   * log becomes) — down to the snapshot's, or the bootstrap. */
  private def configAfter(s: RaftState, merged: Vector[RaftEntry], kept: Int): Long =
    val inNew = merged.lastIndexWhere(_.members.nonEmpty)
    if inNew >= kept then s.snapshotIndex + inNew + 1
    else if s.configIndex <= s.snapshotIndex + kept then s.configIndex
    else
      val inKept = merged.lastIndexWhere(_.members.nonEmpty, kept - 1)
      if inKept >= 0 then s.snapshotIndex + inKept + 1 else s.snapshotIndex

  /**
   * Log compaction (stage 2b, paper §7): the ENGINE has written its
   * state machine as of index `upTo` — applied, hence committed —
   * into `snapshot`, and the log up to there is dropped; the term and
   * the configuration in force at `upTo` move into the snapshot
   * fields, so elections and majorities read the same as before. The
   * bytes are the engine's: the core carries them to a follower that
   * needs them and never reads them. A no-op when `upTo` is not past
   * the current snapshot or not yet committed.
   */
  def compact(s: RaftState, upTo: Long, snapshot: Array[Byte]): RaftState =
    if upTo <= s.snapshotIndex || upTo > s.commitIndex then s
    else
      val term = termAt(s, upTo).getOrElse(s.snapshotTerm)
      val ms =
        if s.configIndex > s.snapshotIndex && s.configIndex <= upTo then entry(s, s.configIndex).members
        else s.snapshotMembers
      s.copy(log = s.log.drop((upTo - s.snapshotIndex).toInt),
        snapshotIndex = upTo, snapshotTerm = term, snapshotMembers = ms, snapshotData = snapshot)

  /** an election timeout fired: become a candidate at the next
   * term, vote for self, ask every member. A node the current
   * configuration does not name does not campaign: it has been
   * removed (or never added) and would only depose a working leader
   * with a term nobody needs — the disruption of thesis §4.2.3,
   * answered here by silence rather than by pre-vote. */
  def startElection(s: RaftState, peers: Set[String]): (RaftState, Vector[RaftOut]) =
    if !members(s, peers)(s.id) then (s, Vector.empty)
    else
      val ns = s.copy(currentTerm = s.currentTerm + 1, votedFor = Some(s.id),
        role = RaftRole.Candidate, votesGranted = Set(s.id), leaderId = None)
      (ns, others(ns, peers).toVector.map(p =>
        RaftOut(p, RaftMsg.RequestVote(ns.currentTerm, ns.id, lastLogIndex(ns), lastLogTerm(ns)))))

  /** a leader's replication tick (also the heartbeat when a peer is
   * fully caught up: entries answers empty) — call after every log
   * change AND periodically to keep leaseless followers current */
  def replicate(s: RaftState, peers: Set[String]): Vector[RaftOut] =
    replicateTo(s, others(s, peers))

  private def replicateTo(s: RaftState, targets: Set[String]): Vector[RaftOut] =
    if s.role != RaftRole.Leader then Vector.empty
    else targets.toVector.map { p =>
      val ni = s.nextIndex.getOrElse(p, lastLogIndex(s) + 1)
      if ni <= s.snapshotIndex then
        // the follower's next entry is inside the snapshot: it gets the whole snapshot
        RaftOut(p, RaftMsg.InstallSnapshot(s.currentTerm, s.id, s.snapshotIndex, s.snapshotTerm,
          s.snapshotMembers, s.snapshotData))
      else
        val prevIdx = ni - 1
        val prevTerm = termAt(s, prevIdx).getOrElse(0L)
        val entries = s.log.drop((prevIdx - s.snapshotIndex).toInt)
        RaftOut(p, RaftMsg.AppendEntries(s.currentTerm, s.id, prevIdx, prevTerm, entries, s.commitIndex))
    }

  /**
   * A membership change on the leader (stage 2a, thesis §4.1): the
   * whole new cluster as ONE configuration entry, in force here at
   * once and on each follower as it arrives. One change at a time —
   * `None` while the previous configuration entry is not yet
   * committed, which is what keeps two disjoint majorities from ever
   * forming — and `None` off the leader or for an empty cluster. A
   * leader removing itself keeps leading until the entry commits,
   * then steps down (see AppendEntriesResp). Not here: the catch-up
   * phase for a joining server — it joins as a voter at once and is
   * brought up to date by ordinary replication. `data` rides along
   * for the transport's own needs (the wire puts the members'
   * addresses there, so every node learns them with the entry);
   * the core never reads it and no state machine applies it.
   */
  def reconfigure(s: RaftState, peers: Set[String], newMembers: Set[String],
                  data: Array[Byte] = Array.empty): Option[(RaftState, Vector[RaftOut])] =
    if s.role != RaftRole.Leader || s.configIndex > s.commitIndex || newMembers.isEmpty then None
    else
      val ns = append(s, RaftEntry(s.currentTerm, data, newMembers.toVector.sorted))
      Some((ns, replicate(ns, peers)))

  /** the ONE state transition: a message in (self-identifying —
   * every RaftMsg names its own sender), the new state plus
   * whatever it answers or forwards. `peers` is this node's
   * BOOTSTRAP view of everyone ELSE in the cluster (never itself),
   * overridden by any configuration entry in the log */
  def handle(s0: RaftState, msg: RaftMsg, peers: Set[String])
  : (RaftState, Vector[RaftOut]) =
    // Raft's own rule, unconditional: SEEING a higher term steps
    // anyone down to a term-less follower, before anything else
    val msgTerm = msg match
      case RaftMsg.RequestVote(t, _, _, _) => t
      case RaftMsg.RequestVoteResp(t, _, _) => t
      case RaftMsg.AppendEntries(t, _, _, _, _, _) => t
      case RaftMsg.AppendEntriesResp(t, _, _, _) => t
      case RaftMsg.Propose(t, _, _, _) => t
      case RaftMsg.Proposed(t, _, _, _, _) => t
      case RaftMsg.InstallSnapshot(t, _, _, _, _, _) => t
    val s =
      if msgTerm > s0.currentTerm then
        s0.copy(currentTerm = msgTerm, votedFor = None, role = RaftRole.Follower, leaderId = None)
      else s0

    msg match
      case RaftMsg.RequestVote(term, cand, lastIdx, lastTerm) =>
        val refuse = (s, Vector(RaftOut(cand, RaftMsg.RequestVoteResp(s.currentTerm, s.id, false))))
        if term < s.currentTerm then refuse
        else
          val upToDate = lastTerm > lastLogTerm(s) ||
            (lastTerm == lastLogTerm(s) && lastIdx >= lastLogIndex(s))
          val canVote = s.votedFor.forall(_ == cand)
          if canVote && upToDate then
            (s.copy(votedFor = Some(cand)),
              Vector(RaftOut(cand, RaftMsg.RequestVoteResp(s.currentTerm, s.id, true))))
          else refuse

      case RaftMsg.RequestVoteResp(term, voter, granted) =>
        if s.role != RaftRole.Candidate || term != s.currentTerm || !granted then (s, Vector.empty)
        else
          // only members' votes count — a removed server's grant is
          // not a vote in the cluster it is no longer part of
          val ms = members(s, peers)
          val votes = (s.votesGranted + voter).filter(ms)
          if votes.size < majority(ms.size) then (s.copy(votesGranted = votes), Vector.empty)
          else
            // won: become leader, optimistic nextIndex, and append the
            // paper's blank no-op (§8) at once — the entry of ITS OWN
            // term whose commit carries every earlier entry with it,
            // so a previous leader's uncommitted entries (a membership
            // change among them) do not wait for the next client
            val os = ms - s.id
            val ni = os.map(_ -> (lastLogIndex(s) + 1)).toMap
            val leader = append(s.copy(role = RaftRole.Leader, votesGranted = votes,
              leaderId = Some(s.id), nextIndex = ni, matchIndex = os.map(_ -> 0L).toMap),
              RaftEntry(s.currentTerm, Array.empty))
            (leader, replicateTo(leader, os))

      case RaftMsg.AppendEntries(term, leader, prevIdx0, prevTerm, entries0, leaderCommit) =>
        val refuse = (s, Vector(RaftOut(leader, RaftMsg.AppendEntriesResp(s.currentTerm, s.id, false, 0))))
        if term < s.currentTerm then refuse
        else
          // a valid leader for our term: acknowledge it (Candidate -> Follower too)
          val st = s.copy(role = RaftRole.Follower, leaderId = Some(leader))
          // a message reaching back into our snapshot: everything up to
          // the snapshot is committed, so it agrees (state-machine
          // safety) — skip that much and go on from the snapshot's edge
          val (prevIdx, entries, logOk) =
            if prevIdx0 < st.snapshotIndex then
              (st.snapshotIndex, entries0.drop((st.snapshotIndex - prevIdx0).toInt), true)
            else (prevIdx0, entries0, termAt(st, prevIdx0).contains(prevTerm))
          if !logOk then (st, Vector(RaftOut(leader, RaftMsg.AppendEntriesResp(st.currentTerm, st.id, false, 0))))
          else
            // splice in the entries, deleting only what CONFLICTS (paper
            // §5.3: an existing entry with the same index and a
            // different term, and everything after it) — never a
            // suffix that merely goes further than this message. The
            // seed harness found the difference: under reordering, an
            // older AppendEntries arriving late truncated entries the
            // follower had already acknowledged and the leader had
            // already committed on that acknowledgement
            val at = (prevIdx - st.snapshotIndex).toInt   // log position of the first new entry
            var agree = 0
            while agree < entries.length && at + agree < st.log.length &&
                  st.log(at + agree).term == entries(agree).term
            do agree += 1
            val merged =
              if agree == entries.length then st.log
              else st.log.take(at + agree) ++ entries.drop(agree)
            // what THIS message establishes — not the log's length, which
            // may go on past it with entries the leader has not vouched for
            val established = math.max(prevIdx0 + entries0.length, st.snapshotIndex)
            val newCommit =
              if leaderCommit > st.commitIndex then math.max(st.commitIndex, math.min(leaderCommit, established))
              else st.commitIndex
            val nst = st.copy(log = merged, commitIndex = newCommit,
              configIndex = configAfter(st, merged, at + agree))
            (nst, Vector(RaftOut(leader, RaftMsg.AppendEntriesResp(nst.currentTerm, nst.id, true, established))))

      case RaftMsg.InstallSnapshot(term, leader, lastIdx, lastTerm, ms, data) =>
        if term < s.currentTerm then
          (s, Vector(RaftOut(leader, RaftMsg.AppendEntriesResp(s.currentTerm, s.id, false, 0))))
        else
          val st = s.copy(role = RaftRole.Follower, leaderId = Some(leader))
          if lastIdx <= st.snapshotIndex then
            // nothing we do not already have: say how far we are
            (st, Vector(RaftOut(leader, RaftMsg.AppendEntriesResp(st.currentTerm, st.id, true, st.snapshotIndex))))
          else
            // paper §7: keep the suffix that follows an entry agreeing
            // with the snapshot's last one; otherwise the snapshot is
            // the whole log now
            val keep =
              if termAt(st, lastIdx).contains(lastTerm) then st.log.drop((lastIdx - st.snapshotIndex).toInt)
              else Vector.empty
            val nst = st.copy(log = keep,
              snapshotIndex = lastIdx, snapshotTerm = lastTerm, snapshotMembers = ms, snapshotData = data,
              commitIndex = math.max(st.commitIndex, lastIdx),
              configIndex = if keep.nonEmpty && st.configIndex > lastIdx then st.configIndex else lastIdx,
              restored = st.restored + 1)
            (nst, Vector(RaftOut(leader, RaftMsg.AppendEntriesResp(nst.currentTerm, nst.id, true, lastIdx))))

      case RaftMsg.AppendEntriesResp(term, follower, success, matchIdx) =>
        if s.role != RaftRole.Leader || term != s.currentTerm then (s, Vector.empty)
        else if !success then
          // log-matching backoff: retry one index earlier
          val ni = math.max(1L, s.nextIndex.getOrElse(follower, 1L) - 1)
          val nst = s.copy(nextIndex = s.nextIndex.updated(follower, ni))
          (nst, replicateTo(nst, Set(follower)))
        else
          val nst = s.copy(
            matchIndex = s.matchIndex.updated(follower, matchIdx),
            nextIndex = s.nextIndex.updated(follower, matchIdx + 1))
          // commit safety (Raft §5.4.2): advance commitIndex to the
          // highest N a MAJORITY of the CURRENT configuration (self
          // included only while a member) has matched, but ONLY when
          // log(N) was written in the CURRENT term — a leader never
          // commits an old term's entry by counting alone; it rides
          // forward with a later entry of its own
          val ms = members(nst, peers)
          val matched = ms.toVector.map(m =>
            if m == nst.id then lastLogIndex(nst) else nst.matchIndex.getOrElse(m, 0L)).sorted
          val n = matched(matched.length - majority(ms.size))
          val committed =
            if n > nst.commitIndex && n >= 1 && termAt(nst, n).contains(nst.currentTerm)
            then n else nst.commitIndex
          val out = nst.copy(commitIndex = committed)
          // a leader whose own removal just committed steps down —
          // after one last heartbeat carrying that commit, so the
          // followers learn it without waiting for the next leader
          if committed > nst.commitIndex && committed >= out.configIndex && !ms(out.id) then
            (out.copy(role = RaftRole.Follower, leaderId = None), replicateTo(out, ms))
          else (out, Vector.empty)

      case RaftMsg.Propose(_, from, n, data) =>
        // a forwarded proposal: the leader appends it as its OWN entry
        // (this term) and replicates at once; anyone else refuses and
        // names the leader it knows, so the proposer can try there
        if s.role == RaftRole.Leader then
          val nst = append(s, RaftEntry(s.currentTerm, data))
          (nst, RaftOut(from, RaftMsg.Proposed(nst.currentTerm, nst.id, n, true, nst.id)) +: replicate(nst, peers))
        else (s, Vector(RaftOut(from, RaftMsg.Proposed(s.currentTerm, s.id, n, false, s.leaderId.getOrElse("")))))

      case RaftMsg.Proposed(_, _, _, _, _) =>
        // the answer is for the node that forwarded, not for the state
        // machine: it changes no term, no log, no vote
        (s, Vector.empty)
