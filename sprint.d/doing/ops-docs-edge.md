- [ ] ops-docs-edge — what ops-docs-vendor-drivers could not do: inside
      ONE sbt build a `Provided` library still reaches a project that
      `dependsOn` the one declaring it (measured 2026-09-25: okay-docs'
      Runtime/fullClasspath has no Mongo or Cassandra driver, okay-ops'
      has both). A source consumer (okay-watch, by `ProjectRef`) must
      `excludeDependencies` them by hand. The lasting fix is the edge:
      okay-ops names okay-docs for ONE type, `Docs.Stats` (in `Ops`'s
      `docs` parameter and `Prom.docs`; only TestProm passes it). Put
      that record where both already reach (it has the shape of
      `Blob.Stats`: an engine and counters), and okay-ops no longer
      depends on okay-docs. Settled when okay-ops' Runtime/fullClasspath
      has no org.mongodb and no com.datastax.oss.
