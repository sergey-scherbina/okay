- [ ] cluster-pool-elastic — stage 5 of specs/cluster-pool.md: peers
      re-resolved at every epoch boundary (stage 13's `Job.rescalable`
      says whether the run may follow, a windowed job refuses by
      name); `okay_pool_queued` through okay-ops's `/metrics` for the
      manager's autoscaler; and a `Lease` over the manager's OWN
      primitive where one exists — Kubernetes `coordination.k8s.io/
      Lease` and Consul sessions, a few dozen lines each behind the
      three-method seam, in okay-pool behind a flag, the ONLY place
      the pool ever speaks to a manager's API. Slurm and YARN have
      none: rank 0 leads with `Lease.solitary`, said rather than
      invented. Trigger: cluster-pool-targets landed.
