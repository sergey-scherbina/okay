- [ ] ops-docs-vendor-drivers — `okay-ops` depends on `okay-docs` for
      ONE type: `Docs.Stats`, in `Ops`'s `docs` parameter and
      `Prom.docs`. Since docs-adapters-merge (2026-09-23) okay-docs on
      the JVM carries both vendor drivers, so every ops user carries
      Mongo and Cassandra and what they bring (Netty, jnr, Typesafe
      config, HdrHistogram, codahale). Measured 2026-09-25 in
      okay-watch's server jar, which calls none of it: 18.9 MB of 64 MB
      compressed; its desktop build deletes them after assembly with
      `zip -d` and a grep that nothing reaches them. Fix: the edge goes
      the other way — `Docs.Stats` rendered by okay-docs into what
      okay-ops already takes (a `Reporting`, or `Prom` lines), or the
      Stats record moved to a module both depend on — then okay-ops no
      longer names okay-docs. Settled when `okayOpsJVM/dependencyClasspath`
      has no mongodb-driver-sync and no java-driver-core, and okay-watch's
      jar is ~19 MB smaller.
