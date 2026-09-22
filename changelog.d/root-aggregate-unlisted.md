## root-aggregate-unlisted - five modules that the matrix never built join the root aggregate; two had already rotted

The pom-jmh-and-chat-version lane found seven modules defined in
build.sbt but absent from the root `.aggregate(...)`, with no reason
written down, so the full matrix and `publishLocal` never touched
them. The operator: sort it out, and docs-cassandra and docs-dynamo
belong in okay-docs.

- okay-ops (JVM + JS), okay-spring, okay-guice, okay-cdi and
  okay-openapi are in the root aggregate now. Each was run alone
  first, and that is where the rot showed:
  - **okay-spring no longer compiled.** `Async` left the core for
    okay-async on 2026-09-18 (core-modules), every aggregated module
    was updated, and this one was never rebuilt, so nothing noticed.
    It now depends on okay-async and okay-platform. 5 tests pass.
  - **okay-ops' JS tests did not link.** `TestOpsSurface` reads
    docs/modules/okay-ops.md through `java.nio.file` but sat in the
    shared test directory. It moved to `src/test/scala-jvm`, beside
    the module's other JVM tests. 21 tests pass on the JVM and 14 on JS.
  - okay-guice (3), okay-cdi (3) and okay-openapi (29) were green as
    they stood.
- okay-docs-dynamo and okay-docs-cassandra were left to a sibling lane,
  docs-adapters-merge, which claimed them first. It landed while this
  lane was in its gate (f45c4b6c) and moved them and okay-docs-mongo
  into okay-docs, as the operator asked. So none of the seven modules
  remains outside the aggregate without a reason.
