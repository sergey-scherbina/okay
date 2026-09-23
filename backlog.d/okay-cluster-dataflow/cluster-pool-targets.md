- [ ] cluster-pool-targets — stages 2 and 3 of specs/cluster-pool.md,
      after cluster-pool-process. `Need.Peers` in specs/deployment.md's
      closed enum (a spec edit on purpose): on `cluster` a HEADLESS
      Service beside the Deployment (ready pods only) and
      `OKAY_POOL_SERVICE`; on `laptop` `deploy.replicas` and compose's
      own DNS; on `host` N units and the list in the EnvironmentFile;
      `gcp`/`azure`/`render`/`railway` REFUSE by name (no per-replica
      address) naming the nearest target that works. Then the kind
      harness that dataflow stage 12 ("the network", blocked on
      machines) has waited for: N pods, a submission, `kubectl delete
      pod` of a member and of the coordinator, `kubectl scale` between
      epochs — `Live`, docker-dependent. Stage 3 adds `nomad`, `yarn`
      (YARN Services API, Hadoop 3.1+), `slurm`, `swarm`, AWS `batch`
      as pure renderings, each gated by its format's real parser and
      `sh -n`, no account needed; plus the grep test that no manager's
      name or client is in okay-cluster's or okay-pool's main sources.
      Mesos is a trigger, not a row (Apache Attic; dropped by Spark 4.0
      and Flink 1.17).
