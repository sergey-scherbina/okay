## ops-docs-vendor-drivers - a server no longer carries Mongo and Cassandra

okay-ops depends on okay-docs for one type (`Docs.Stats`, in `Ops` and
`Prom.docs`), and since docs-adapters-merge okay-docs on the JVM carried
both vendor drivers on its compile scope — so every server built on
okay-ops carried mongodb-driver-sync and the Cassandra java-driver-core,
with the Netty, jnr, Typesafe config, HdrHistogram and codahale they
bring. Measured in okay-watch's server jar, which calls none of it:
18.9 MB of 64 MB compressed.

- The two drivers are `Provided` in okay-docs (build.sbt): okay-docs
  compiles and tests against them, nothing downstream receives them. A
  program that builds a `MongoDocs` or a `CassandraDocs` names its
  driver, as a JDBC user names theirs.
- docs/modules/okay-docs.md, okay-docs/README.md and specs/data.md say so.
- WHAT IT DOES NOT DO, found the same day from okay-watch: inside ONE
  build `Provided` still travels through `dependsOn` — okay-docs' own
  Runtime classpath is clean, okay-ops' is not. So it holds for a
  PUBLISHED consumer (the POM says provided) and not for a source
  consumer (a `ProjectRef`, okay-watch's way): okay-watch excludes the
  three organisations in its own build and its jar went 69.9 → 53.0 MB.
  The edge itself is backlog.d/build/ops-docs-edge.
- Gate: `affected master` (build.sbt, so the whole family), 7261 test
  results, GREEN, no warnings.

Landed as 8b4af6ad5.
