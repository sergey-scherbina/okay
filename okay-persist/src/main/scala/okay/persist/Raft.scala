package okay.persist

/**
 * Own Raft, stage 0 (specs/consensus.md, persist-raft): the
 * consensus ALGORITHM's core state machine — leader election and
 * log replication, safety proven by simulation — as a pure value
 * transition, no engine, no network, no Store yet. `Election`
 * (specs/consensus.md, the reduction) does not change when this
 * eventually lands as a `RaftStore` behind a `Topic`: the control
 * log's total order is all Election ever asked an engine for, and a
 * Raft-replicated log is one more way to produce it.
 *
 * Deliberately NOT here (filed as the next slices under the same
 * BACKLOG name): the `Store`/`Topic` engine wrapper (turning this
 * state machine into something `Election` can construct a topic
 * over), a real network transport, log compaction/snapshotting,
 * and cluster membership changes. This slice is the textbook
 * core (Ongaro & Ousterhout, Figure 2) minus those — proven correct
 * here so the engine wrapper has an honest foundation to sit on.
 */

/** one log entry: the term it was appended under (the log matching
 * property needs it) plus an opaque payload — what the payload
 * MEANS is the eventual Store engine's business, not this core's */
final case class RaftEntry(term: Long, data: Array[Byte])

enum RaftRole:
  case Follower, Candidate, Leader

/**
 * Everything one node knows: the persistent state the paper names
 * (currentTerm, votedFor, log) plus the volatile bookkeeping a
 * candidate/leader needs (votesGranted, nextIndex, matchIndex).
 * `log` is 1-indexed conceptually — `log(i - 1)` is Raft's index i —
 * matching the paper exactly so its proofs read across unchanged.
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
  matchIndex: Map[String, Long] = Map.empty)

enum RaftMsg:
  case RequestVote(term: Long, candidateId: String, lastLogIndex: Long, lastLogTerm: Long)
  case RequestVoteResp(term: Long, from: String, voteGranted: Boolean)
  case AppendEntries(term: Long, leaderId: String, prevLogIndex: Long, prevLogTerm: Long,
                     entries: Vector[RaftEntry], leaderCommit: Long)
  case AppendEntriesResp(term: Long, from: String, success: Boolean, matchIndex: Long)

/** one outgoing message, addressed */
final case class RaftOut(to: String, msg: RaftMsg)

object Raft:

  def lastLogIndex(s: RaftState): Long = s.log.length.toLong
  def lastLogTerm(s: RaftState): Long = s.log.lastOption.map(_.term).getOrElse(0L)
  private def majority(clusterSize: Int): Int = clusterSize / 2 + 1

  /** an election timeout fired: become a candidate at the next
   * term, vote for self, ask every peer */
  def startElection(s: RaftState, peers: Set[String]): (RaftState, Vector[RaftOut]) =
    val ns = s.copy(currentTerm = s.currentTerm + 1, votedFor = Some(s.id),
      role = RaftRole.Candidate, votesGranted = Set(s.id), leaderId = None)
    (ns, peers.toVector.map(p =>
      RaftOut(p, RaftMsg.RequestVote(ns.currentTerm, ns.id, lastLogIndex(ns), lastLogTerm(ns)))))

  /** a leader's replication tick (also the heartbeat when a peer is
   * fully caught up: entries answers empty) — call after every log
   * change AND periodically to keep leaseless followers current */
  def replicate(s: RaftState, peers: Set[String]): Vector[RaftOut] =
    if s.role != RaftRole.Leader then Vector.empty
    else peers.toVector.map { p =>
      val ni = s.nextIndex.getOrElse(p, lastLogIndex(s) + 1)
      val prevIdx = ni - 1
      val prevTerm = if prevIdx <= 0 then 0L else s.log(prevIdx.toInt - 1).term
      val entries = s.log.drop(prevIdx.toInt)
      RaftOut(p, RaftMsg.AppendEntries(s.currentTerm, s.id, prevIdx, prevTerm, entries, s.commitIndex))
    }

  /** the ONE state transition: a message in (self-identifying —
   * every RaftMsg names its own sender), the new state plus
   * whatever it answers or forwards. `peers` is this node's view of
   * everyone ELSE in the cluster (never itself) */
  def handle(s0: RaftState, msg: RaftMsg, peers: Set[String])
  : (RaftState, Vector[RaftOut]) =
    // Raft's own rule, unconditional: SEEING a higher term steps
    // anyone down to a term-less follower, before anything else
    val msgTerm = msg match
      case RaftMsg.RequestVote(t, _, _, _) => t
      case RaftMsg.RequestVoteResp(t, _, _) => t
      case RaftMsg.AppendEntries(t, _, _, _, _, _) => t
      case RaftMsg.AppendEntriesResp(t, _, _, _) => t
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
          val votes = s.votesGranted + voter
          if votes.size < majority(peers.size + 1) then (s.copy(votesGranted = votes), Vector.empty)
          else
            // won: become leader, optimistic nextIndex, replicate at once
            val ni = peers.map(_ -> (lastLogIndex(s) + 1)).toMap
            val leader = s.copy(role = RaftRole.Leader, votesGranted = votes,
              leaderId = Some(s.id), nextIndex = ni, matchIndex = peers.map(_ -> 0L).toMap)
            (leader, replicate(leader, peers))

      case RaftMsg.AppendEntries(term, leader, prevIdx, prevTerm, entries, leaderCommit) =>
        val refuse = (s, Vector(RaftOut(leader, RaftMsg.AppendEntriesResp(s.currentTerm, s.id, false, 0))))
        if term < s.currentTerm then refuse
        else
          // a valid leader for our term: acknowledge it (Candidate -> Follower too)
          val st = s.copy(role = RaftRole.Follower, leaderId = Some(leader))
          val logOk = prevIdx == 0 ||
            (prevIdx <= lastLogIndex(st) && st.log(prevIdx.toInt - 1).term == prevTerm)
          if !logOk then (st, Vector(RaftOut(leader, RaftMsg.AppendEntriesResp(st.currentTerm, st.id, false, 0))))
          else
            // truncate any conflicting suffix, then splice in entries —
            // idempotent: a resent identical AppendEntries changes nothing
            val merged = st.log.take(prevIdx.toInt) ++ entries
            val newCommit =
              if leaderCommit > st.commitIndex then math.min(leaderCommit, merged.length.toLong)
              else st.commitIndex
            val nst = st.copy(log = merged, commitIndex = newCommit)
            (nst, Vector(RaftOut(leader, RaftMsg.AppendEntriesResp(nst.currentTerm, nst.id, true, merged.length.toLong))))

      case RaftMsg.AppendEntriesResp(term, follower, success, matchIdx) =>
        if s.role != RaftRole.Leader || term != s.currentTerm then (s, Vector.empty)
        else if !success then
          // log-matching backoff: retry one index earlier
          val ni = math.max(1L, s.nextIndex.getOrElse(follower, 1L) - 1)
          val nst = s.copy(nextIndex = s.nextIndex.updated(follower, ni))
          (nst, replicate(nst, Set(follower)))
        else
          val nst = s.copy(
            matchIndex = s.matchIndex.updated(follower, matchIdx),
            nextIndex = s.nextIndex.updated(follower, matchIdx + 1))
          // commit safety (Raft §5.4.2): advance commitIndex to the
          // highest N a MAJORITY (self included) has matched, but
          // ONLY when log(N) was written in the CURRENT term — a
          // leader never commits an old term's entry by counting
          // alone; it rides forward with a later entry of its own
          val matched = (nst.matchIndex.values.toVector :+ lastLogIndex(nst)).sorted
          val n = matched(matched.length - majority(peers.size + 1))
          val committed =
            if n > nst.commitIndex && n >= 1 && nst.log(n.toInt - 1).term == nst.currentTerm
            then n else nst.commitIndex
          (nst.copy(commitIndex = committed), Vector.empty)
