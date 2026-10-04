## okay2-persist-raft - replication, election and the Raft core in okay2-persist

Lane 2 of okay2-persist-rest (operator: "do everything needed for okay2,
all at once"; specs/okay2.md stage 56, docs/okay2.md section 37), on JVM,
Scala.js and Scala Native.

- `Replicated`: a coordinator over replica stores behind `Topic`. It has
  quorum acks, reads cut at the high-water mark, epoch fencing recorded
  as an ops event, the idempotent producer window, and `promote` and
  `replicate`.
- `Election`: leadership as a fold of a control topic, with leases and
  the operator's override.
- `Raft`: the pure consensus core, with pre-vote, membership changes,
  compaction and InstallSnapshot.
- Suites: TestReplicated, ElectionSuite (run over memory as TestElection
  and over the file arbiter as TestElectionFile), TestElectionReplicated
  (`Live`, as okay-persist tags it), TestRaft, and TestRaftSim, which
  sweeps 40 seeds with loss, reordering, a partition and a membership
  change. All 40 converge and every acked proposal is kept.
