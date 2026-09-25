## cluster-forked-stall - the stalled suite named, out of the default gate, and its waits dump the workers

- The three stalled gates of 2026-09-25 were NOT four forked test JVMs.
  The saved process trees show one test fork at 0% CPU with four
  `okay.cluster.WorkerMain` CHILDREN, spawned a second after it, also
  at 0%. That is TestDistributed's "FOUR REAL PROCESSES". sbt's own
  dump shows it waiting for the fork to exit, and the fork's test
  waited on the workers with no deadline (`getLines().find(...)`).
- TestDistributed was the one binding, process-spawning suite in the
  DEFAULT gate. TestFailure's and TestFederation's process tests were
  already Live. Its two binding tests are Live now.
- `Workers` (test): `spawn`, `ports` and `within`, each with a deadline.
  Past the deadline the test fails with every worker's thread dump
  (`jcmd Thread.print`). All three process-spawning tests use it.
  TestWorkers pins the three behaviours without starting a process.
- What made the workers stall is still unknown, because they were
  never dumped. The next stall, in `integrationTest`, dumps them itself:
  backlog okay-cluster-dataflow/cluster-worker-stall-cause.
- Also: the `kubectl proxy` beside those stalls was okay-pool's
  in-process TestKubeLease (a child of sbt, not of the fork), fixed
  separately by kube-lease-proxy-leak.
