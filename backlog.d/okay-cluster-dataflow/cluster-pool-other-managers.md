- [ ] cluster-pool-other-managers — stage 3 of specs/cluster-pool.md,
      after cluster-pool-targets (LANDED 2026-09-24). `nomad`, `yarn`
      (YARN Services API, Hadoop 3.1+), `slurm`, `swarm`, AWS `batch`
      as pure renderings, each gated by its format's real parser and
      `sh -n`, no account needed — the same bar specs/deployment.md's
      own stages 2 and 3 set (a real TOML/YAML/JSON parser or
      `terraform validate`, never a golden file). Plus the grep test
      that no manager's name or client is in okay-cluster's or
      okay-pool's main sources (Claim 1). Mesos is a trigger, not a
      row (Apache Attic; dropped by Spark 4.0 and Flink 1.17).
