- [ ] cluster-forked-stall — PRIORITY: MEDIUM (gates without a verdict).
      Two full gates on 2026-09-25 (stacked-shift0, instances-unify) STALLED
      in okay-cluster's forked tests. The watchdog's dumps show sbt's
      `ForkTests` waiting, four forked JVMs of okay-cluster/test-classes at
      0% CPU after TestFederation/TestLeak, and a `kubectl proxy` child of
      sbt. A rerun passed both times. It is the known "gate hangs on idle
      runners" class, but in one module, twice. THE LANE: dump the FORKED
      JVMs (not only sbt) at the next stall, and find which suite holds
      them. The `kubectl proxy` suggests a cluster-pool test whose proxy
      outlives it. Dumps were at
      /var/folders/.../okay-gate.1cmy0fzDVN.stall.{json,ps}. (2026-09-25)
      THIRD sighting, same shape (okay-compress-zstd-speed's "affected
      master staged", 2026-09-25 18:59): four forked okay-cluster JVMs at
      0% after TestFederation/TestLeak, `kubectl proxy --port=52700` a
      child of sbt, 389 test results; dump
      okay-gate.tISAxr50or.stall.{json,ps}. The forks were killed with
      the run, so they are still undumped.
