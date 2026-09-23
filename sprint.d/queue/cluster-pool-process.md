- [ ] cluster-pool-process — stage 1 of specs/cluster-pool.md: the
      new JVM module `okay-pool` (depends on okay-cluster.jvm,
      okay-http, okay-ops, okay-resilience, okay-conf — the engine's
      own graph stays at okay-codec). `Pool.main` reads `PoolConf`
      (defaults → file → environment), loads the registrars, serves
      the worker protocol and the HTTP door on ONE port; peers =
      `Discovery(service)` ∪ static list minus this member, each a
      `Served.reconnecting`; `POST /pool/jobs/{name}` (202 / 400 with
      the field / 404 with the names), `GET /pool/runs/{id}`,
      `GET /pool/jobs`, `GET /pool/peers`; a build fingerprint in the
      protocol, refused both ways. Gate: N processes on one machine
      with a static list answer `Flows.fan`'s value; a member killed
      mid-run; a mismatched fingerprint refused; a streaming
      re-submission after the coordinator died resumes at the next
      epoch. Spawning suites are `Live` (the federation two-process
      timeout is the precedent). Docs with the lane
      (docs/modules/okay-pool.md). Operator ask 2026-09-23.
