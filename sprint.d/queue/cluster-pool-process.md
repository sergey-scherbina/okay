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
      protocol, refused both ways. EVERY submission runs under
      `Cluster.leading` with a journal (client-supplied or
      pool-generated, and it IS the run id) — no bare in-memory
      `Cluster.run` — so `PoolConf.store` names one `(runId =>
      (Checkpoint, Lease))` factory and `Pool.main` refuses the
      in-memory default once more than one peer is configured. Gate: N
      processes on one machine with a static list answer `Flows.fan`'s
      value; a member killed mid-run; a mismatched fingerprint refused;
      the member that accepted a submission is killed before it
      finishes and a `GET /pool/runs/{id}` against a DIFFERENT member
      resumes it from the journal — no second submission. Spawning
      suites are `Live` (the federation two-process timeout is the
      precedent). Docs with the lane (docs/modules/okay-pool.md).
      Operator ask 2026-09-23; journal made mandatory 2026-09-24 after
      review found a plain submission's status was a single point of
      failure on the accepting member.
