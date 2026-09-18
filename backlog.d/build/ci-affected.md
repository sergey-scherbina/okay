- [x] ci-affected — DONE 2026-09-16. The numbers: every Actions run in
      the visible history cancelled by the next push, two at the
      six-hour limit, while the same `sbt test` is two minutes warm on
      the box. `affected <ref|a..b> [task]` and `family <platform>` as
      sbt commands in project/ (Scala, no plugin): a file belongs to a
      project by its source and resource directories, dependents are
      closed over the classpath graph, the root aggregate is the
      bound. ci.yml runs `affected` per push with a `target/` cache
      restored from the nearest previous key, and the family nightly,
      one job per platform. Measured: see CHANGELOG.
