- [ ] cluster-pool-numbers — stage 6 of specs/cluster-pool.md, Claim 3
      measured rather than argued: submission-to-`Done` latency of a
      small job on a warm pool on kind, and the fixed/marginal split
      docs/benchmarks.md §20 uses, BESIDE Spark cluster mode on the
      same kind (okay-spark already carries the dependency; Spark pays
      executor pod scheduling per application). One machine, containers,
      not a datacentre — recorded with the same honesty §20's
      distributed section has now. Trigger: cluster-pool-targets landed
      and a quiet box.
      STARTED AND PAUSED (triage 2026-09-25 evening): a lane claimed it at
      01:00, deployed okay-pool on kind (cluster `okay-pool-test`, still
      up) and paused at 06:57 because a load-80 box made kubectl's API
      calls time out; no heartbeat since. Its uncommitted work — PoolDeploy,
      Bench, FileStoreRegistrar, okay-pool/deploy, okay-deploy target
      changes, Dockerfiles — is preserved UNGATED on branch
      feature/cluster-pool-numbers (a84cd8bd1, worktree
      ../okay-wt-cluster-pool-numbers). Resume from there on a quiet box.
