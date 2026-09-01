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

- [ ] two nodes claim the same epoch concurrently through one
      control log: exactly one wins on EVERY node's fold; the loser
      observes its loss and does not promote
- [ ] a leader that cannot renew its lease is taken over after
      expiry + skew; nothing acknowledged is lost (the promote path
      catches the successor up, as stage 2 already proves)
- [ ] a paused-and-resumed old leader is fenced by epoch on its
      first append; the rejection is an ops event (stage 2's test,
      re-run under automatic election)
- [ ] the operator record wins over any concurrent automatic claim
      at the same epoch, on every fold
- [ ] the control log over KafkaStore and over a FileStore arbiter
      passes the same election battery (the two-engine acceptance,
      the house move)
- [ ] arbiter down: data partitions keep serving; failover waits;
      the operator path still works — availability degradation is
      visible in stats, correctness untouched

## Out of scope

- own Raft (RaftStore) — filed separately when a deployment names
  the need; the reduction above is designed so it slots in as an
  engine
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

(after the implementation claim — the election battery on both
control-log engines, the flapping bound measured, stats surfaced)
