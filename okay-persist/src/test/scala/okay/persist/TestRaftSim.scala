package okay.persist

import okay.codec.Cbor

/**
 * The seed-swept simulation of the Raft core (specs/consensus.md, "the
 * Sim-driven fuzz harness"): a discrete-event simulator over the PURE
 * `Raft.handle`/`startElection`/`replicate`/`compact`, no threads, no
 * sockets, no fibers — the core is a function, so its network is a
 * priority queue of events and its clock a number.
 *
 * Per seed: five nodes, randomized election timeouts and heartbeats,
 * every message delayed by a random amount (so reordered), dropped
 * with a probability, a partition cut at a random moment and healed
 * later, client proposals injected at whoever is leader, and — stage
 * 2b — each node running a STATE MACHINE (the texts it applied) that
 * it snapshots and compacts its log to every so often, so a node
 * that fell behind a compacted stretch can only be caught up by
 * InstallSnapshot. After EVERY delivered event the safety properties
 * are checked on what the state machines saw — election safety, log
 * matching, state-machine safety, leader completeness — and after
 * the partition heals and the network turns lossless, progress as
 * Raft promises it: every ACKED proposal (committed on the leader
 * that accepted it — what a client is told) is in every node's
 * state machine, the cluster converges on one commit index, and a
 * late proposal gets acked. What a minority-side leader accepted
 * during the cut and never committed is not a promise and may be
 * lost, as the paper says. A failing seed is printed; rerunning it
 * replays the exact interleaving byte for byte, because nothing
 * here reads a clock or a thread.
 */
class TestRaftSim extends munit.FunSuite {

  final case class Event(at: Long, seq: Long, node: String, kind: Kind)
  enum Kind:
    case Deliver(msg: RaftMsg)
    case ElectionTimeout(armed: Long)
    case Heartbeat
    case Propose(data: String)
    /** an operator asks the node (the leader, one hopes) for a new cluster */
    case Reconfigure(members: Set[String])
    /** the engine's periodic look at whether to snapshot and compact */
    case Compact

  final class Sim(seed: Long, var ids: Vector[String] = Vector("0", "1", "2", "3", "4"),
                  var dropRate: Double = 0.1, maxDelay: Long = 40, electionMs: Long = 150,
                  heartbeatMs: Long = 50, compactEvery: Long = 200, compactAfter: Int = 4) {
    private val rnd = new scala.util.Random(seed)
    /** each node's BOOTSTRAP view of the others (what it was started
     * knowing); a configuration entry in its log overrides it */
    private var boot: Map[String, Set[String]] = ids.map(i => i -> (ids.toSet - i)).toMap
    var states: Map[String, RaftState] = ids.map(i => i -> RaftState(id = i)).toMap
    /** each node's state machine: the texts of the entries it applied,
     * in log order (a no-op or a configuration entry applies as "") */
    var machine: Map[String, Vector[String]] = ids.map(_ -> Vector.empty[String]).toMap
    /** how far each state machine has applied */
    private var applied: Map[String, Long] = ids.map(_ -> 0L).toMap
    /** nodes shut down by the operator after their removal: their
     * events are dropped, their frozen state is nobody's business */
    var halted: Set[String] = Set.empty
    def live: Vector[String] = ids.filterNot(halted)
    /** how many membership changes the leader refused (an earlier one
     * still uncommitted, or asked off the leader) — a stat, not a fault */
    var refusedChanges = 0
    /** snapshots taken and snapshots installed from a leader, across the run */
    var compactions = 0
    var installs = 0
    private var seq = 0L
    private val queue = collection.mutable.PriorityQueue.empty[Event](using Ordering.by[Event, (Long, Long)](e => (e.at, e.seq)).reverse)
    var now = 0L
    /** the term each leader was elected in, for election safety */
    var leadersByTerm: Map[Long, Set[String]] = Map.empty
    /** what a leader ACCEPTED (appended to its log) — not yet a
     * promise: a leader deposed before commit loses it, by the paper's
     * own rules. The promise is `acked`: a proposal the accepting
     * leader COMMITTED (what a client is told), which must then be in
     * every node's state machine for ever. */
    var accepted: Vector[String] = Vector.empty
    private var inFlight: Vector[(String, Long, String)] = Vector.empty   // (leader, index, data)
    var acked: Set[String] = Set.empty

    /** the text at Raft index `idx` on `id`, from the state machine
     * (applied) or the log; None when compacted away or past the end */
    private def textAt(id: String, idx: Long): Option[String] =
      val st = states(id)
      if idx >= 1 && idx <= applied(id) then Some(machine(id)((idx - 1).toInt))
      else if idx > st.snapshotIndex && idx <= Raft.lastLogIndex(st) then Some(text(st.log((idx - st.snapshotIndex - 1).toInt)))
      else None

    private def settleAcks(): Unit =
      val (done, still) = inFlight.partition { (l, idx, data) => states(l).commitIndex >= idx && textAt(l, idx).contains(data) }
      // an in-flight proposal whose entry the leader has LOST (overwritten
      // by a new leader) will never be acked: drop it from the flight
      val alive = still.filter { (l, idx, data) => textAt(l, idx).contains(data) }
      acked = acked ++ done.map(_._3)
      inFlight = alive

    /** the engine's side after every transition on `id`: a restored
     * snapshot resets the state machine to its bytes; then everything
     * newly committed is applied in order */
    private def applyCommitted(id: String, before: RaftState): Unit =
      val st = states(id)
      if st.restored != before.restored then
        machine = machine.updated(id, Cbor.read[Vector[String]](st.snapshotData).getOrElse(throw AssertionError(s"seed $seed: $id: unreadable snapshot")))
        applied = applied.updated(id, st.snapshotIndex)
        installs += 1
        assert(machine(id).length.toLong == st.snapshotIndex, s"seed $seed: $id: snapshot of ${machine(id).length} entries at index ${st.snapshotIndex}")
      while applied(id) < st.commitIndex do
        val next = applied(id) + 1
        machine = machine.updated(id, machine(id) :+ text(st.log((next - st.snapshotIndex - 1).toInt)))
        applied = applied.updated(id, next)

    /** links cut by the current partition, as (from, to) */
    var cut: Set[(String, String)] = Set.empty
    /** the highest election-timeout arming per node: an older timeout is stale */
    private var armed: Map[String, Long] = ids.map(_ -> 0L).toMap

    private def schedule(delay: Long, node: String, kind: Kind): Unit =
      seq += 1
      queue.enqueue(Event(now + delay, seq, node, kind))

    private def armElection(node: String): Unit =
      armed = armed.updated(node, armed(node) + 1)
      schedule(electionMs + rnd.nextInt(electionMs.toInt), node, Kind.ElectionTimeout(armed(node)))

    ids.foreach(armElection)
    ids.foreach(n => schedule(heartbeatMs, n, Kind.Heartbeat))
    ids.foreach(n => schedule(compactEvery + rnd.nextInt(compactEvery.toInt), n, Kind.Compact))

    private def send(from: String, outs: Vector[RaftOut]): Unit =
      outs.foreach { o =>
        if !cut((from, o.to)) && rnd.nextDouble() >= dropRate then
          schedule(1 + rnd.nextInt(maxDelay.toInt), o.to, Kind.Deliver(o.msg))
      }

    private def apply(node: String, f: RaftState => (RaftState, Vector[RaftOut])): Unit =
      val was = states(node)
      val (ns, out) = f(was)
      states = states.updated(node, ns)
      if ns.role == RaftRole.Leader && was.role != RaftRole.Leader then
        leadersByTerm = leadersByTerm.updated(ns.currentTerm, leadersByTerm.getOrElse(ns.currentTerm, Set.empty) + node)
      applyCommitted(node, was)
      send(node, out)

    def leader: Option[String] = live.find(states(_).role == RaftRole.Leader)
    def membersOf(id: String): Set[String] = Raft.members(states(id), boot(id))

    /** a fresh server, started knowing the current cluster (as an
     * operator would configure it), nobody's member until a leader
     * says so; it runs its own timers from now on */
    def addNode(id: String): Unit =
      boot = boot.updated(id, live.toSet)
      ids = ids :+ id
      states = states.updated(id, RaftState(id = id))
      machine = machine.updated(id, Vector.empty)
      applied = applied.updated(id, 0L)
      armed = armed.updated(id, 0L)
      armElection(id)
      schedule(heartbeatMs, id, Kind.Heartbeat)
      schedule(compactEvery, id, Kind.Compact)

    /** the operator shuts a removed node down */
    def halt(id: String): Unit = halted = halted + id

    /** one event; false when the queue is empty */
    def step(): Boolean =
      if queue.isEmpty then false
      else
        val e = queue.dequeue()
        now = e.at
        if !halted(e.node) then e.kind match
          case Kind.Deliver(msg) =>
            apply(e.node, s => Raft.handle(s, msg, boot(e.node)))
            msg match
              case _: RaftMsg.AppendEntries | _: RaftMsg.InstallSnapshot => armElection(e.node)
              case _ => ()
          case Kind.ElectionTimeout(a) =>
            if a == armed(e.node) && states(e.node).role != RaftRole.Leader then
              apply(e.node, s => Raft.startElection(s, boot(e.node)))
              armElection(e.node)
          case Kind.Heartbeat =>
            if states(e.node).role == RaftRole.Leader then send(e.node, Raft.replicate(states(e.node), boot(e.node)))
            schedule(heartbeatMs, e.node, Kind.Heartbeat)
          case Kind.Propose(data) =>
            states(e.node).role match
              case RaftRole.Leader =>
                apply(e.node, s =>
                  val ns = Raft.append(s, RaftEntry(s.currentTerm, data.getBytes("UTF-8")))
                  (ns, Raft.replicate(ns, boot(e.node))))
                accepted = accepted :+ data
                inFlight = inFlight :+ (e.node, Raft.lastLogIndex(states(e.node)), data)
              case _ => ()   // a client that hit a follower retries later
          case Kind.Reconfigure(members) =>
            Raft.reconfigure(states(e.node), boot(e.node), members) match
              case Some((ns, out)) => states = states.updated(e.node, ns); send(e.node, out)
              case None => refusedChanges += 1
          case Kind.Compact =>
            // the engine: once enough is applied past the last snapshot,
            // write the state machine and drop the log up to it
            val st = states(e.node)
            if applied(e.node) - st.snapshotIndex >= compactAfter then
              val upTo = applied(e.node)
              val ns = Raft.compact(st, upTo, Cbor.write(machine(e.node).take(upTo.toInt)))
              if ns.snapshotIndex == upTo then compactions += 1
              states = states.updated(e.node, ns)
            schedule(compactEvery, e.node, Kind.Compact)
        settleAcks()
        true

    def propose(data: String): Unit = leader.foreach(l => schedule(0, l, Kind.Propose(data)))
    def reconfigure(members: Set[String]): Unit = leader.foreach(l => schedule(0, l, Kind.Reconfigure(members)))
    /** every live node counts `members` as the cluster and has committed that entry */
    def settledOn(members: Set[String]): Boolean =
      live.forall(id => membersOf(id) == members && states(id).commitIndex >= states(id).configIndex)
    def partition(side: Set[String]): Unit =
      cut = (for a <- ids; b <- ids if a != b && side(a) != side(b) yield (a, b)).toSet
    def heal(): Unit = cut = Set.empty
    /** the last stretch of a run is lossless: "eventually" needs a
     * network that eventually delivers */
    def lossless(): Unit = dropRate = 0.0
    def runUntil(t: Long): Unit = while now < t && step() do ()
    def random(n: Int): Int = rnd.nextInt(n)

    // ---- safety, checked after every event ----------------------
    private def text(e: RaftEntry) = String(e.data, "UTF-8")
    /** everything `id` holds, by index: its state machine (applied ==
     * committed after every step) and then its log past the commit */
    private def full(id: String): Vector[String] =
      val st = states(id)
      machine(id) ++ st.log.drop((st.commitIndex - st.snapshotIndex).toInt).map(text)
    def check(): Unit =
      // election safety: at most one leader per term
      leadersByTerm.foreach((t, ls) => assert(ls.size <= 1, s"seed $seed: term $t has leaders $ls"))
      val ids = live
      // log matching: two logs with the same (index, term) agree on
      // everything before it — over the indexes both still hold
      for a <- ids; b <- ids if a < b do
        val sa = states(a); val sb = states(b)
        val lo = math.max(sa.snapshotIndex, sb.snapshotIndex) + 1
        val hi = math.min(Raft.lastLogIndex(sa), Raft.lastLogIndex(sb))
        var i = hi
        while i >= lo && Raft.termAt(sa, i) != Raft.termAt(sb, i) do i -= 1
        if i >= lo then
          var j = lo
          while j <= i do
            assert(Raft.termAt(sa, j) == Raft.termAt(sb, j) && textAt(a, j) == textAt(b, j),
              s"seed $seed: logs of $a and $b agree at $i but differ at $j")
            j += 1
      // state-machine safety: what two machines applied at one index is one entry
      for a <- ids; b <- ids if a < b do
        val m = math.min(machine(a).length, machine(b).length)
        var i = 0
        while i < m do
          assert(machine(a)(i) == machine(b)(i), s"seed $seed: $a and $b applied different entries at ${i + 1}")
          i += 1
      // leader completeness: the leader of the HIGHEST term holds every
      // entry any node has applied. A cut-off old leader still calling
      // itself one is exactly who the property does not cover — its
      // uncommitted tail is what the paper says may be lost
      val leaders = ids.filter(states(_).role == RaftRole.Leader)
      leaders.maxByOption(states(_).currentTerm).foreach { l =>
        val mine = full(l)
        for n <- ids do
          val theirs = machine(n)
          assert(mine.length >= theirs.length, s"seed $seed: leader $l lacks entries $n applied")
          var i = 0
          while i < theirs.length do
            assert(mine(i) == theirs(i), s"seed $seed: leader $l disagrees with $n's applied ${i + 1}")
            i += 1
      }
  }

  private def scenario(seed: Long): Sim =
    val sim = Sim(seed)
    var next = 0
    // a first leader, then proposals under churn
    sim.runUntil(2000); sim.check()
    for round <- 1 to 6 do
      for _ <- 1 to 3 do
        sim.propose(s"e${next}"); next += 1
        sim.runUntil(sim.now + 30 + sim.random(120)); sim.check()
      if round == 2 then
        // cut a minority off, then the majority side keeps going
        val minority = Set("0", "1")
        sim.partition(minority)
      if round == 4 then sim.heal()
    // after healing, a lossless stretch: the cluster must converge, and
    // a proposal made now must be acked and committed everywhere
    sim.lossless()
    sim.runUntil(sim.now + 3000); sim.check()
    sim.propose("late"); sim.runUntil(sim.now + 3000); sim.check()
    sim

  test("safety on every seed: one leader per term, logs match, committed entries agree and are never lost") {
    for seed <- 1L to 40L do
      val sim = scenario(seed)
      sim.check()
  }

  test("progress on every seed: after the partition heals, every acked proposal is applied everywhere, the cluster converges, and a late proposal is acked") {
    val results = (1L to 40L).map { seed =>
      val sim = scenario(seed)
      val commits = sim.states.values.map(_.commitIndex).toSet
      val machines = sim.live.map(id => sim.machine(id).toSet)
      // what Raft promises: every ACKED proposal (committed on the leader
      // that accepted it — what a client is told) is in every node's
      // state machine; the cluster converges once the network is
      // lossless; and the late proposal, made on a whole network, is
      // acked. An entry a minority leader accepted during the cut and
      // never committed is not a promise, and the paper says so.
      val ackedEverywhere = machines.forall(ts => sim.acked.forall(ts.contains))
      val late = sim.acked.contains("late")
      (seed, commits.size == 1, ackedEverywhere, late, sim.accepted.length, sim.acked.size, commits.head, sim.compactions, sim.installs)
    }
    val bad = results.filterNot((_, one, all, late, _, _, _, _, _) => one && all && late)
    println(f"[raft-sim] 40 seeds, 5 nodes, drop 10%%, delays 1-40, a minority cut rounds 2-4, then lossless: ${results.count(_._2)} converged, ${results.count(_._3)} kept every ack, ${results.count(_._4)} acked the late one; accepted ${results.map(_._5).sum}, acked ${results.map(_._6).sum}, committed per seed ${results.map(_._7).min}..${results.map(_._7).max}; snapshots taken ${results.map(_._8).sum}, installed ${results.map(_._9).sum} (seeds with an install: ${results.count(_._9 > 0)})")
    assertEquals(bad.map(_._1), Vector.empty, s"seeds that did not converge, lost an ack, or never acked the late proposal: $bad")
    assert(results.map(_._9).sum > 0, "no seed ever needed InstallSnapshot: the sweep did not exercise it")
  }

  /** stage 2a: a sixth node joins, then whoever leads removes itself,
   * under the same loss and reordering; the removed node is shut down
   * once its removal has committed somewhere, as an operator would */
  private def membership(seed: Long): (Sim, Boolean, Boolean) =
    val sim = Sim(seed)
    var next = 0
    def churn(rounds: Int): Unit =
      for _ <- 1 to rounds do
        sim.propose(s"e${next}"); next += 1
        sim.runUntil(sim.now + 30 + sim.random(120)); sim.check()
    def change(members: Set[String]): Boolean =
      var tries = 0
      while !sim.settledOn(members) && tries < 8 do
        sim.reconfigure(members); tries += 1
        sim.runUntil(sim.now + 600); sim.check()
      sim.settledOn(members)
    sim.runUntil(2000); sim.check()
    churn(3)
    sim.addNode("5")
    val grew = change(Set("0", "1", "2", "3", "4", "5"))
    churn(3)
    val gone = sim.leader.getOrElse("0")
    val shrank = change(Set("0", "1", "2", "3", "4", "5") - gone)
    // the operator shuts the removed node down (it has stopped
    // campaigning by itself: a non-member never starts an election)
    sim.halt(gone)
    churn(3)
    sim.lossless()
    sim.runUntil(sim.now + 3000); sim.check()
    sim.propose("late"); sim.runUntil(sim.now + 3000); sim.check()
    (sim, grew, shrank)

  test("membership on every seed: a node joins and the leader removes itself under loss; safety holds and the rest go on") {
    val results = (1L to 40L).map { seed =>
      val (sim, grew, shrank) = membership(seed)
      val commits = sim.live.map(sim.states(_).commitIndex).toSet
      val machines = sim.live.map(id => sim.machine(id).toSet)
      val ackedEverywhere = machines.forall(ts => sim.acked.forall(ts.contains))
      val late = sim.acked.contains("late")
      (seed, grew, shrank, commits.size == 1, ackedEverywhere, late, sim.refusedChanges, sim.leadersByTerm.size, sim.installs)
    }
    val bad = results.filterNot((_, grew, shrank, one, all, late, _, _, _) => grew && shrank && one && all && late)
    println(f"[raft-sim] membership, 40 seeds, drop 10%%, delays 1-40: ${results.count(_._2)} grew to six, ${results.count(_._3)} shrank without their leader, ${results.count(_._4)} converged, ${results.count(_._5)} kept every ack, ${results.count(_._6)} acked the late one; changes refused ${results.map(_._7).sum}, terms per seed ${results.map(_._8).min}..${results.map(_._8).max}, snapshots installed ${results.map(_._9).sum} (a joiner restored from one on ${results.count(_._9 > 0)} seeds)")
    assertEquals(bad.map(_._1), Vector.empty, s"seeds that failed a membership change, lost an ack, or did not converge: $bad")
  }

  test("a seed replays byte for byte") {
    val a = scenario(7L); val b = scenario(7L)
    assertEquals(a.states.map((k, v) => k -> (v.currentTerm, v.commitIndex, v.snapshotIndex, v.log.map(e => String(e.data, "UTF-8")))),
      b.states.map((k, v) => k -> (v.currentTerm, v.commitIndex, v.snapshotIndex, v.log.map(e => String(e.data, "UTF-8")))))
    assertEquals(a.machine, b.machine)
    assertEquals(a.leadersByTerm, b.leadersByTerm)
  }
}
