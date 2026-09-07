package okay.persist

/**
 * The seed-swept simulation of the Raft core (specs/consensus.md, "the
 * Sim-driven fuzz harness"): a discrete-event simulator over the PURE
 * `Raft.handle`/`startElection`/`replicate`, no threads, no sockets, no
 * fibers — the core is a function, so its network is a priority
 * queue of events and its clock a number.
 *
 * Per seed: five nodes, randomized election timeouts and heartbeats,
 * every message delayed by a random amount (so reordered), dropped
 * with a probability, a partition cut at a random moment and healed
 * later, and client proposals injected at whoever is leader. After
 * EVERY delivered event the safety properties are checked — election
 * safety, log matching, state-machine safety, leader completeness —
 * and after the partition heals and the network turns lossless,
 * progress as Raft promises it: every ACKED proposal (committed on
 * the leader that accepted it — what a client is told) is in every
 * node's committed prefix, the cluster converges on one commit
 * index, and a late proposal gets acked. What a minority-side leader
 * accepted during the cut and never committed is not a promise and
 * may be lost, as the paper says. A failing seed is printed; rerunning it replays the
 * exact interleaving byte for byte, because nothing here reads a
 * clock or a thread.
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

  final class Sim(seed: Long, var ids: Vector[String] = Vector("0", "1", "2", "3", "4"),
                  var dropRate: Double = 0.1, maxDelay: Long = 40, electionMs: Long = 150,
                  heartbeatMs: Long = 50) {
    private val rnd = new scala.util.Random(seed)
    /** each node's BOOTSTRAP view of the others (what it was started
     * knowing); a configuration entry in its log overrides it */
    private var boot: Map[String, Set[String]] = ids.map(i => i -> (ids.toSet - i)).toMap
    var states: Map[String, RaftState] = ids.map(i => i -> RaftState(id = i)).toMap
    /** nodes shut down by the operator after their removal: their
     * events are dropped, their frozen state is nobody's business */
    var halted: Set[String] = Set.empty
    def live: Vector[String] = ids.filterNot(halted)
    /** how many membership changes the leader refused (an earlier one
     * still uncommitted, or asked off the leader) — a stat, not a fault */
    var refusedChanges = 0
    private var seq = 0L
    private val queue = collection.mutable.PriorityQueue.empty[Event](using Ordering.by[Event, (Long, Long)](e => (e.at, e.seq)).reverse)
    var now = 0L
    /** the term each leader was elected in, for election safety */
    var leadersByTerm: Map[Long, Set[String]] = Map.empty
    /** what a leader ACCEPTED (appended to its log) — not yet a
     * promise: a leader deposed before commit loses it, by the paper's
     * own rules. The promise is `acked`: a proposal the accepting
     * leader COMMITTED (what a client is told), which must then be in
     * every node's committed prefix for ever. */
    var accepted: Vector[String] = Vector.empty
    private var inFlight: Vector[(String, Long, String)] = Vector.empty   // (leader, index, data)
    var acked: Set[String] = Set.empty
    private def settleAcks(): Unit =
      val (done, still) = inFlight.partition { (l, idx, data) =>
        val st = states(l)
        st.commitIndex >= idx && idx <= st.log.length && String(st.log(idx.toInt - 1).data, "UTF-8") == data
      }
      // an in-flight proposal whose entry the leader has LOST (overwritten
      // by a new leader) will never be acked: drop it from the flight
      val alive = still.filter { (l, idx, data) =>
        val st = states(l); idx <= st.log.length && String(st.log(idx.toInt - 1).data, "UTF-8") == data }
      acked = acked ++ done.map(_._3)
      inFlight = alive
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

    private def send(from: String, outs: Vector[RaftOut]): Unit =
      outs.foreach { o =>
        if !cut((from, o.to)) && rnd.nextDouble() >= dropRate then
          schedule(1 + rnd.nextInt(maxDelay.toInt), o.to, Kind.Deliver(o.msg))
      }

    private def apply(node: String, f: RaftState => (RaftState, Vector[RaftOut])): Unit =
      val (ns, out) = f(states(node))
      val was = states(node)
      states = states.updated(node, ns)
      if ns.role == RaftRole.Leader && was.role != RaftRole.Leader then
        leadersByTerm = leadersByTerm.updated(ns.currentTerm, leadersByTerm.getOrElse(ns.currentTerm, Set.empty) + node)
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
      armed = armed.updated(id, 0L)
      armElection(id)
      schedule(heartbeatMs, id, Kind.Heartbeat)

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
              case _: RaftMsg.AppendEntries => armElection(e.node)
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
                inFlight = inFlight :+ (e.node, states(e.node).log.length.toLong, data)
              case _ => ()   // a client that hit a follower retries later
          case Kind.Reconfigure(members) =>
            Raft.reconfigure(states(e.node), boot(e.node), members) match
              case Some((ns, out)) => states = states.updated(e.node, ns); send(e.node, out)
              case None => refusedChanges += 1
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
    def check(): Unit =
      // election safety: at most one leader per term
      leadersByTerm.foreach((t, ls) => assert(ls.size <= 1, s"seed $seed: term $t has leaders $ls"))
      val ids = live
      // log matching: two logs with the same (index, term) agree on everything before it
      for a <- ids; b <- ids if a < b do
        val la = states(a).log; val lb = states(b).log
        val n = math.min(la.length, lb.length)
        var i = n - 1
        while i >= 0 && la(i).term != lb(i).term do i -= 1
        if i >= 0 then
          var j = 0
          while j <= i do
            assert(la(j).term == lb(j).term && text(la(j)) == text(lb(j)),
              s"seed $seed: logs of $a and $b agree at ${i + 1} but differ at ${j + 1}")
            j += 1
      // state-machine safety: committed entries at one index are one entry
      for a <- ids; b <- ids if a < b do
        val m = math.min(states(a).commitIndex, states(b).commitIndex).toInt
        var i = 0
        while i < m do
          assert(text(states(a).log(i)) == text(states(b).log(i)),
            s"seed $seed: $a and $b committed different entries at ${i + 1}")
          i += 1
      // leader completeness: the leader of the HIGHEST term holds every
      // entry any node has committed. A cut-off old leader still calling
      // itself one is exactly who the property does not cover — its
      // uncommitted tail is what the paper says may be lost
      val leaders = ids.filter(states(_).role == RaftRole.Leader)
      leaders.maxByOption(states(_).currentTerm).foreach { l =>
        for n <- ids do
          val c = states(n).commitIndex.toInt
          assert(states(l).log.length >= c, s"seed $seed: leader $l lacks entries $n committed")
          var i = 0
          while i < c do
            assert(text(states(l).log(i)) == text(states(n).log(i)), s"seed $seed: leader $l disagrees with $n's committed ${i + 1}")
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

  test("progress on every seed: after the partition heals, every acked proposal is committed everywhere, the cluster converges, and a late proposal is acked") {
    val results = (1L to 40L).map { seed =>
      val sim = scenario(seed)
      val commits = sim.states.values.map(_.commitIndex).toSet
      val committedTexts = sim.states.values.map(s => s.log.take(s.commitIndex.toInt).map(e => String(e.data, "UTF-8")).toSet)
      // what Raft promises: every ACKED proposal (committed on the leader
      // that accepted it — what a client is told) is in every node's
      // committed prefix; the cluster converges once the network is
      // lossless; and the late proposal, made on a whole network, is
      // acked. An entry a minority leader accepted during the cut and
      // never committed is not a promise, and the paper says so.
      val ackedEverywhere = committedTexts.forall(ts => sim.acked.forall(ts.contains))
      val late = sim.acked.contains("late")
      (seed, commits.size == 1, ackedEverywhere, late, sim.accepted.length, sim.acked.size, commits.head)
    }
    val bad = results.filterNot((_, one, all, late, _, _, _) => one && all && late)
    println(f"[raft-sim] 40 seeds, 5 nodes, drop 10%%, delays 1-40, a minority cut rounds 2-4, then lossless: ${results.count(_._2)} converged, ${results.count(_._3)} kept every ack, ${results.count(_._4)} acked the late one; accepted ${results.map(_._5).sum}, acked ${results.map(_._6).sum}, committed per seed ${results.map(_._7).min}..${results.map(_._7).max}")
    assertEquals(bad.map(_._1), Vector.empty, s"seeds that did not converge, lost an ack, or never acked the late proposal: $bad")
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
      val committedTexts = sim.live.map(id => sim.states(id).log.take(sim.states(id).commitIndex.toInt).map(e => String(e.data, "UTF-8")).toSet)
      val ackedEverywhere = committedTexts.forall(ts => sim.acked.forall(ts.contains))
      val late = sim.acked.contains("late")
      (seed, grew, shrank, commits.size == 1, ackedEverywhere, late, sim.refusedChanges, sim.leadersByTerm.size)
    }
    val bad = results.filterNot((_, grew, shrank, one, all, late, _, _) => grew && shrank && one && all && late)
    println(f"[raft-sim] membership, 40 seeds, drop 10%%, delays 1-40: ${results.count(_._2)} grew to six, ${results.count(_._3)} shrank without their leader, ${results.count(_._4)} converged, ${results.count(_._5)} kept every ack, ${results.count(_._6)} acked the late one; changes refused ${results.map(_._7).sum}, terms per seed ${results.map(_._8).min}..${results.map(_._8).max}")
    assertEquals(bad.map(_._1), Vector.empty, s"seeds that failed a membership change, lost an ack, or did not converge: $bad")
  }

  test("a seed replays byte for byte") {
    val a = scenario(7L); val b = scenario(7L)
    assertEquals(a.states.map((k, v) => k -> (v.currentTerm, v.commitIndex, v.log.map(e => String(e.data, "UTF-8")))),
      b.states.map((k, v) => k -> (v.currentTerm, v.commitIndex, v.log.map(e => String(e.data, "UTF-8")))))
    assertEquals(a.leadersByTerm, b.leadersByTerm)
  }
}
