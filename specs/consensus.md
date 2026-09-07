# Consensus: who may advance an epoch

## Overview

Stage 2 of specs/persist.md built everything about leadership
EXCEPT choosing the leader: epochs fence deposed writers, the
high-water mark bounds what any reader can observe, `promote`
catches the successor up before it leads, and the ops topic audits
every change. What remains is one question — **who MAY call
promote, and when** — and stage 2 answers it with an operator. This
spec is about removing the operator from the loop without removing
the guarantees, and it exists as its own spec because automatic
election is the genuinely hard part of distributed systems: the
failure it must survive is the network splitting while both halves
stay alive, each concluding "the leader died, I shall elect a new
one" — two leaders, forked history. The industry's settled answer
(Paxos, then Raft as its teachable form) is MAJORITY: leadership
requires votes from more than half the nodes, and a split network
has at most one half with a majority.

The design constraint inherited from stage 2, kept deliberately:
election changes WHO may advance an epoch, never what an epoch is.
Nothing below re-opens fencing, the hwm, or the promote path.

## The reduction: election is a fold of a control log

The move this stack has made sixteen times today is made once
more: consensus is CONSUMED as a totally-ordered log, and where
that log comes from is an engine choice.

Observe what a total order buys. Give every node a shared CONTROL
TOPIC (single partition — total order within a partition is the
stage-0 guarantee) and make leadership changes RECORDS:

```scala
enum Claim derives Schema:
  /** "I claim partition p at epoch e" */
  case Take(partition: Int, epoch: Long, node: String)
  /** the current leader's heartbeat: "mine until t" */
  case Lease(partition: Int, epoch: Long, node: String, untilMillis: Long)
  /** the operator's word, which always wins */
  case Operator(partition: Int, epoch: Long, node: String)
```

Every node folds the control topic. The rule is one sentence: **the
FIRST `Take` at epoch e wins epoch e** — and because the log is
totally ordered, every node's fold agrees on which was first, with
no voting protocol of our own. A losing claimant discovers it lost
by reading its own claim land second. A leader keeps leadership by
appending `Lease` records; a candidate may append `Take(p, e+1, …)`
only after observing the last lease of epoch e EXPIRE. The winner
of a fold calls stage 2's `promote` on itself — the same code path
the operator uses, now driven by the fold.

This is the classic replicated-state-machine reduction run in
reverse: instead of building an election to get an ordered log, an
ordered log is used to get an election. The hard problem does not
vanish — it moves into the question "who orders the control
topic?" — and that is exactly where this stack wants it, because
ordering a topic is what Store engines DO:

- **KafkaStore (available today)**: Kafka's controller quorum
  (KRaft) already runs Raft; a single-partition Kafka topic is a
  totally-ordered, majority-replicated log. Election delegated to
  the engine that did the twenty years — the stage-3 hatch paying
  off a second time. This is the RECOMMENDED first road.
- **A single arbiter node (dev/small deployments)**: one
  `FileStore` behind persist-wire is a total order too. The
  arbiter is a single point of failure for ELECTIONS ONLY — data
  partitions keep serving under their current leaders when the
  arbiter is down, and the operator path still works. Stated
  honestly: this is availability of failover traded away, not
  correctness.
- **Own Raft (the long game)**: a `RaftStore` — the consensus
  algorithm implemented once, as a Store engine whose single
  partition is the replicated log. When it lands, the election
  machinery above does not change BY CONSTRUCTION, which is the
  entire argument for the reduction. Its own claim will carry its
  own spec section (log matching, election timeouts, snapshotting
  of the control fold) — months of careful work, honestly priced,
  and justified only when a deployment cannot run Kafka and has
  outgrown the arbiter.

## Leases and clocks, honestly

A lease is a promise about TIME, and distributed clocks lie. The
caveats are stated, not hidden:

- A candidate honors a lease until `untilMillis` PLUS a declared
  skew allowance (config, default generous). A leader stops
  serving `Ack.Replicated` appends when its own clock says its
  lease expired and it could not renew — the two conservatisms
  overlap, so a paused-then-resumed leader (GC, laptop lid) finds
  itself fenced by epoch before it can damage anything: the fence,
  not the clock, is the correctness mechanism. Leases only decide
  LIVENESS — when a takeover may start.
- Failure detection is the lease going unrenewed, nothing subtler
  v1 (no phi-accrual, no gossip). A flapping network yields
  flapping elections bounded by lease length; the lease length is
  the operator's knob and the spec says so.

## What stays from stage 2

- `promote` remains the ONLY way an epoch advances; election just
  automates the caller.
- The operator record (`Claim.Operator`) outranks the fold: a
  human's promote appends it, and every node's fold treats it as
  winning its epoch unconditionally — automation must never lock
  the operator out.
- Fencing and the hwm carry ALL safety. If every clock in the
  fleet lies at once, the worst outcome is a rejected append and
  an ops event, not forked history.

## Behavior (for the implementation claim)

- [x] two nodes claim the same epoch concurrently through one
      control log: exactly one wins on EVERY node's fold; the loser
      observes its loss and does not promote (three folds agree;
      and a winner that never LEASES loses the seat — the liveness
      rule caught the test assuming otherwise)
- [x] a leader that cannot renew its lease is taken over after
      expiry + skew; nothing acknowledged is lost (the promote path
      catches the successor up, as stage 2 already proves; renewal
      holds the seat, injectable clock makes expiry a test)
- [x] a paused-and-resumed old leader is fenced by epoch on its
      first append; the rejection is an ops event (stage 2's test,
      re-run under automatic election)
- [x] the operator record wins over any concurrent automatic claim
      at the same epoch, on every fold (even landing SECOND)
- [x] the control log over KafkaStore and over a FileStore arbiter
      passes the same election battery (the two-engine acceptance,
      the house move) — ElectionSuite unchanged over memory, the
      file arbiter, and live Kafka (5 tests each)
- [x] arbiter down: data partitions keep serving; failover waits;
      the operator path still works — a dead control topic fails
      the claim loudly, Replicated keeps appending at quorum, and
      the stage-2 manual promote never passes through the control
      log at all

## Own Raft (persist-raft) — staged

Filed for months, taken up 2026-09-03 (operator: "start it anyway,"
understanding a session lands a slice, not the whole climb). Staged
explicitly rather than attempted whole, matching how this stack
prices every large claim:

- **Stage 0 — the algorithm's core state machine, LANDED
  2026-09-03.** `okay.persist.Raft` (`RaftState`, `RaftMsg`,
  `RaftEntry`, `Raft.handle`/`startElection`/`replicate`): a PURE
  value transition, no engine, no network, no `Store` yet — the
  textbook core (Ongaro & Ousterhout, Figure 2) minus everything
  staged below. `RaftState.log` is 1-indexed to match the paper's
  own proofs. Every RPC (RequestVote/AppendEntries, request and
  response) is `handle`'s one case; a higher term seen on ANY
  message steps a node down first, unconditionally, before the
  message is otherwise handled — Raft's own rule, applied once at
  the top rather than repeated per case.
  Proven by seven tests, DRIVEN EXPLICITLY (a test calls
  `electionTimeout`/`heartbeat`/`deliverAll` itself — no wall clock,
  no autonomous timer, matching `TestElectionReplicated`'s own
  manual-clock style): a lone candidate wins a majority and becomes
  leader; two simultaneous candidates never BOTH become leader in
  one term (a genuine split vote resolves on a retried, higher-term
  election); a client entry replicates to a majority and the leader
  commits it; a heartbeat propagates `commitIndex` to followers; a
  follower whose log diverged at an OLDER term (never a same-term
  same-index difference — Leader Completeness forbids that from ever
  legitimately arising) is corrected by the next AppendEntries, not
  merely appended past; a stale-term message is refused untouched, a
  higher-term message steps a leader down; and the Figure 8 trap —
  an entry from a PREVIOUS term is never committed by majority count
  alone, only ridden forward by a later entry of the leader's OWN
  term. `okayPersistJVM` full suite 95/95 (the reduction's existing
  battery unaffected — this stage touches no file it depends on).
- **Stage 1a — the peer-to-peer wire transport, LANDED 2026-09-03.**
  `okay.persist.RaftWire.Node` (JVM-only, `okay-persist/src/main/
  scala-jvm`): real `ServerSocket`s, real threads, the SAME
  `[len:int32][CBOR]` framing `Wire.scala` already uses for the
  client-facing wire — reused here for NODE-TO-NODE `RaftMsg`
  exchange (`RaftEntry`/`RaftMsg` now `derives Schema`). One-shot
  connections (connect, write one frame, close) rather than a
  persistent connection pool per peer — simple and correct first,
  not yet optimized; Raft's own retry-by-heartbeat already tolerates
  a dropped send. A background tick thread drives REAL wall-clock
  election timeouts (randomized, per node) and leader heartbeats —
  the first REAL non-determinism this algorithm has run under,
  stage 0's tests having driven everything by explicit tick.
  `propose(data)` is the client seam: succeeds only on the current
  leader, `false` otherwise (no forwarding to the real leader yet —
  a stage 1b refinement). `onCommit(index, entry)` fires once per
  newly committed index, in order — the exact seam a future `Store`
  wrapper applies to its own topic.
  Deliberately NOT here: the `Store`/`Topic` wrapper itself (so
  `Election` still cannot construct a topic over this — that is
  stage 1b), and persistent storage for `currentTerm`/`votedFor` (a
  real process crash forgets them here — Raft's OWN safety proof
  assumes stable storage for exactly those two fields; `RaftWire`
  runs safely today only as long as no participating node crashes
  mid-term, stated not hidden).
- **Stage 1b — the `Store`/`Topic` engine wrapper, and persistent
  `currentTerm`/`votedFor`, LANDED 2026-09-07.** `okay.persist.
  RaftStore` (JVM-only, beside `RaftWire`) IS a `Store`, so
  `Election` constructs a `Topic` over it without changing — the
  reduction's claim, by construction. The shape is the paper's
  replicated state machine: an `append` is proposed to the leader as
  one log entry (`RaftStore.Op.Append`: topic, partition, key, value,
  the proposer's id and sequence, CBOR through the existing
  `RaftEntry.data`), and every node applies each committed entry to
  its LOCAL store in log order through stage 1a's `onCommit` seam —
  so the local stores agree, and a read served from the local store
  never shows a record a failover could unwrite (only committed
  entries reach it: `Replicated`'s high-water-mark guarantee, by a
  different road). Every `append` waits for ITS entry to commit and
  answers the local offset it was applied at; a proposal that finds
  no majority within `commitWaitMs` throws `NotCommitted` rather than
  pretending. (As landed, a follower's append threw `NotLeader` — the
  same-day slice below carries it to the leader instead.) Topics are
  declared per node, as configuration.
  `RaftWire.Stable` is the stable storage the proof assumes: `load()`
  once at start, `save(term, votedFor)` inside the lock BEFORE any
  message the transition produced is sent (a reply that outran its
  own record is the double vote); `Stable.file(path)` is one small
  file replaced by rename, `Stable.memory` for tests. `TestStable`
  (default gate: the file round-trips, absent reads as the initial
  state, a node started over a remembered term 5 begins at 5 with
  its vote) and `TestRaftStore` (`Live`, real sockets, as
  `TestRaftWire`: three stores, an append on the leader applied on
  all three at offsets 0 and 1, a follower's append refused by name,
  the leader killed and the survivors electing, accepting and
  applying). Not here: the commit-wait as an `Ack` level rather than
  a timeout, and stage 2.
- **Forwarding, LANDED 2026-09-07 (persist-raft-forward).** A
  follower's proposal is carried to the leader on the node wire: one
  message pair, `RaftMsg.Propose(term, from, n, data)` and
  `Proposed(term, from, n, accepted, leader)`, both `handle`'s cases
  — the leader appends a forwarded proposal as its OWN entry (this
  term) and replicates at once, anyone else refuses and names the
  leader it knows. The proposer needs no answer for correctness: its
  pending wait completes when the entry commits on ITS node, keyed by
  proposer and sequence as before; the refusal, when it comes, fails
  the wait with the leader named (`NotLeader(leader)`) rather than
  letting it time out. `Node.propose(n, data)` is true when the
  proposal is appended here or on its way, false only when no leader
  is known — an election in progress — which is the one case
  `RaftStore.append` still throws `NotLeader` for. The Live suite's
  first test now appends on a follower too, and every node applies
  it at the next offset.
- **Stage 2a — membership changes, LANDED 2026-09-07
  (raft-membership).** The single-server change of Ongaro's thesis
  §4.1, the variant its authors recommend over joint consensus: a
  configuration entry (`RaftEntry.members`, the whole cluster) in
  the log, in force from the moment it is appended — committed or
  not — on every node that holds it; `RaftState.configIndex` names
  the latest one, kept by `Raft.append` and by AppendEntries'
  splice, so a truncated change REVERTS with its entry. `peers`
  became the bootstrap view; majorities, votes and replication run
  over `Raft.members`. One change at a time: `Raft.reconfigure`
  answers `None` while the previous configuration entry is
  uncommitted — the rule that keeps two disjoint majorities from
  forming. A leader removing itself leads until the entry commits,
  sends one last heartbeat carrying that commit, then steps down; a
  node the configuration does not name does not campaign (thesis
  §4.2.3's disruption, answered by silence rather than pre-vote). On
  the wire: `RaftWire.Node.reconfigure(cluster)` and
  `RaftStore.reconfigure` — the entry's `data` carries the members'
  addresses (`RaftWire.Address`), so the LOG is the directory and
  every node that holds the entry can reach every member it names;
  the Live test's first run showed why: only the leader that
  accepted the change knew the newcomer's address, its successor
  could not heartbeat the newcomer, the newcomer campaigned, and
  the terms spun. Addresses grow, never shrink (a removed node is
  owed that last heartbeat); the store applies neither
  configuration entries nor no-ops. Two things the simulator forced
  on the way, both the paper's own rules the core had skipped:
  (1) **the blank no-op at the start of every term** (§8): five of
  forty seeds never committed the leader's own removal — the leader
  was deposed after appending it, and its successor, refusing a
  second change while the first was uncommitted and with no client
  entry of its own term to ride, waited for ever; with the no-op,
  zero refusals on forty seeds. (2) **delete only CONFLICTING
  entries in AppendEntries** (§5.3): the core truncated everything
  after `prevLogIndex` unconditionally, so under reordering an
  older AppendEntries arriving late cut entries the follower had
  already acknowledged and the leader had already committed on that
  acknowledgement — commitIndex past the end of the log, the safety
  seed the no-op's timing exposed. Fixed as the paper states it,
  with `matchIndex` and the follower's commit bound now what the
  message ESTABLISHED (`prevLogIndex + entries.length`), not the
  log's length, which may go on past it with entries the leader
  never vouched for. Not here: the catch-up (non-voting) phase for
  a joining server — it joins as a voter at once and is brought up
  to date by ordinary replication, which the sweep shows suffices
  at these sizes — and pre-vote.
- **Stage 2b — log compaction and InstallSnapshot, LANDED
  2026-09-07 (raft-compaction).** Paper §7, with the bytes the
  ENGINE's: `Raft.compact(s, upTo, snapshot)` takes an index the
  engine has applied (so committed) and the engine's own image of
  its state machine there, drops the log up to it, and keeps the
  term and the configuration in force at that index in the
  snapshot fields (`snapshotIndex/Term/Members/Data`), so
  elections and majorities read the same as before; the core never
  reads the bytes. Every log access goes through index arithmetic
  against the snapshot (`termAt`, `lastLogIndex` = snapshot +
  log length); an AppendEntries reaching back into a follower's
  snapshot skips what the snapshot covers (state-machine safety
  says it agrees) and goes on from the edge. A leader whose
  `nextIndex` for a follower is inside its snapshot sends
  `RaftMsg.InstallSnapshot` (one message, not chunked) instead of
  entries; the follower keeps the suffix following an entry that
  agrees with the snapshot's last one, else discards its log, and
  bumps `RaftState.restored` — the engine's cue to reset its state
  machine to the bytes at that index before applying anything
  later. The answer is an `AppendEntriesResp` whose `matchIndex` is
  the snapshot's edge, the same "what this message established"
  the ordinary reply carries. On the wire: `Node.compact(upTo,
  snapshot)` and `onRestore(index, bytes)`; a configuration that
  arrives inside a snapshot carries no addresses (the bytes are the
  engine's), so a node restored that way reaches what it was
  started knowing plus what later entries teach it. The simulator
  now runs a state machine per node — the texts it applied — which
  it snapshots and compacts to every 200 ms once four entries past
  the last snapshot, and the safety properties are asserted on what
  the MACHINES saw rather than on log prefixes; the partition
  scenario forces snapshots by construction (the minority misses
  what the majority commits and compacts): forty seeds, 541
  snapshots taken, 81 installed on 32 seeds, safety clean, every
  ack kept; the membership sweep restored a joiner from a snapshot
  on 33 of 40 seeds. Three hand-written cases in `TestRaft`
  (compaction keeps term, configuration and bytes; a follower that
  missed a compacted stretch is restored and goes on from the edge;
  an older snapshot is a no-op and an agreeing suffix survives) and
  a Live wire test (two nodes commit and compact, a third starts
  late and is restored through `onRestore`, then applies only what
  came after). Not here: `RaftStore`'s own snapshot (stage 2c — its
  state machine is the local `Store`, whose image is a store dump,
  not a byte string yet), chunked snapshots, snapshot-carried
  addresses.
- **The typestate note, still open** (asked by the user, 2026-09-01):
  the ROLE protocol (Follower → Candidate → Leader, each with its
  own legal actions) is the textbook typestate case; `PState` (the
  type-changing state paramonad) could make "a follower may not
  append as leader" a COMPILE error rather than the plain
  `if s.role != RaftRole.Leader` runtime check stage 0 uses — its
  per-op cost is irrelevant at election rates. Not done in stage 0:
  the plain enum kept the FIRST slice's diff small and legible
  against the paper; revisit once stage 1 gives typestate something
  real to guard (a network handler that must not even COMPILE an
  AppendEntries send from a Follower).
- **The seed-swept simulation harness, LANDED 2026-09-07
  (raft-sim-fuzz).** `TestRaftSim` (default gate, every platform: the
  core is pure) is a discrete-event simulator over `Raft.handle` /
  `startElection` / `replicate` — not fibers on `Sim`, because the
  core is a function of messages and timeouts, so its network is a
  priority queue of events and its clock a number; `Sim` stays the
  machinery for programs with effects. Per seed: five nodes,
  randomized election timeouts and heartbeats, every message delayed
  by 1–40 (so reordered) and dropped at 10%, a minority of two cut
  off in round 2 and healed in round 4, three proposals a round at
  whoever leads, then a lossless stretch and one late proposal. After
  EVERY event the four safety properties are asserted — election
  safety (one leader per term), log matching, state-machine safety
  (committed entries at one index agree), leader completeness (a
  leader holds every entry any node committed) — and at the end the
  promise Raft actually makes: every ACKED proposal (committed on the
  leader that accepted it, what a client is told) is in every node's
  committed prefix, the cluster has converged on one commit index,
  and the late proposal was acked. Forty seeds: safety held on every
  event of every seed; 40 converged, 40 kept every ack, 40 acked the
  late one; 670 proposals accepted, 560 acked — the 110 are entries a
  minority-side leader accepted during the cut and never committed,
  lost on rejoin exactly as the paper says an uncommitted entry may
  be, which the harness's first version wrongly counted as losses
  until the property was stated as the paper states it. A failing
  seed prints itself and replays byte for byte; the suite asserts
  that too (seed 7 twice, equal states). Since stage 2a the same
  harness also sweeps membership: a sixth node joins and whoever
  leads removes itself, under the same loss and reordering, forty
  seeds — 40 grew, 40 shrank without their leader, 40 converged,
  every ack kept, zero changes refused; and it found the two
  skipped rules recorded under stage 2a, which is the harness doing
  what it was built for. Since stage 2b every node in it runs a
  state machine it snapshots and compacts to, and the properties
  are asserted on what the machines saw; InstallSnapshot fires on
  most seeds (the sweep asserts that it fired at all).

## Out of scope

- membership changes / rebalancing partitions across nodes — the
  control log can carry assignment records later; static
  assignment stands until then
- Byzantine behavior — crash-stop is the model, as everywhere in
  this stack

## Decisions

- **Election as a fold of a control log, not a protocol of our
  own** — total order is the primitive this stack already
  guarantees and already knows how to source from three engines;
  first-claim-wins over a total order needs no votes, no terms of
  our own, no new wire messages. Rejected: implementing Raft
  before consuming consensus (months of work standing between the
  business and automatic failover that Kafka already provides);
  rejected: per-partition election groups (the CockroachDB shape —
  election traffic and quorum bookkeeping per partition, complexity
  this scale has not earned).
- **Delegation first, arbiter for dev, Raft as a future engine** —
  the P4/P7 bridges-AND-own decision, applied to consensus itself.
  Rejected: ZooKeeper/etcd client dependencies (a dependency tree
  for what a topic already gives us through engines we own).
- **Leases decide liveness, epochs decide safety** — the fence is
  the correctness mechanism and it already exists; clocks are only
  trusted to schedule takeovers, with the skew allowance declared.
  Rejected: leaning on synchronized clocks for correctness (the
  lie every postmortem warns about).
- **The operator always outranks automation** — `Claim.Operator`
  wins its epoch on every fold; a stuck election never locks a
  human out. Rejected: automation-only failover.

## Results

Landed (persist-election, 2026-09-01): `Election` in okay-persist
(cross-platform — it consumes total order and a clock, nothing
else). The fold is ~40 lines: first-Take-wins per epoch, Operator
overrides even landing second, a deposed leader's Lease is noise.
`tryTakeover` answers from the FOLD, not the append — the claim
lands, the node reads back whether it was first — and the winner
immediately leases so a racing claimant sees no vacancy. The
battery: 5 suite tests × three control-log engines (memory, the
FileStore arbiter, live Kafka — unchanged, which was the claim),
plus 3 integration tests driving stage 2's promote (loss-free
takeover, epoch fencing, the arbiter-down degradation). One truth
the tests taught back: a winner that never leases loses the seat —
liveness working as specified. RaftStore remains the filed future
engine; flapping-bound measurement joins the first deployment.
