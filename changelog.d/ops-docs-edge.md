## ops-docs-edge - okay-ops no longer depends on okay-docs

What ops-docs-vendor-drivers could not do: inside one sbt build a
`Provided` library still reaches a project that `dependsOn` the one
declaring it, so every server built on okay-ops from source still
carried okay-docs' Mongo and Cassandra drivers. okay-ops named okay-docs
for one type, `Docs.Stats`.

- `Docs.prom` (okay-docs) renders the `okay_docs_*{name,engine}`
  counters itself — the same names, help and escaping.
- `Ops.router`/`Ops.routes` take `more: Vector[() => String]`,
  Prometheus text a module renders itself, read per scrape; the `docs`
  parameter and `Prom.docs` are gone (only TestProm used them).
- build.sbt: okayOps drops okayDocs. TestDocsProm (2) in okay-docs.
- specs/data.md, docs/modules/okay-docs.md say so.
- Gate: `affected master`, 7267 test results, GREEN, no warnings.

Landed as 1feb44c00.
