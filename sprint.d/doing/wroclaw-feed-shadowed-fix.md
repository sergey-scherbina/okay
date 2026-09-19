- [ ] wroclaw-feed-shadowed-fix — compare/BUGS.md's wroclaw-feed-shadowed,
      diagnosed and fixed: `OkayLane.scala`'s `import okay.*` was
      shadowing the same-package `okay.wroclaw.Feed` (Gtfs.scala) with
      core's `type Feed[W] = Unit ! Writer % W`. One-line fix:
      `import okay.{Feed as _, *}` (Scala 3 import-exclusion syntax)
      excludes `Feed` from the wildcard so the same-package case class
      wins again. Verified: `sbt compare/compile` goes from 30 errors
      to clean.

      WHY NOW: blocks okay-spark's own test compile (`okaySpark
      .dependsOn(..., compare % "test->compile")`), discovered while
      trying to verify a Spark JDK25 fix (spark-4-2-0-jdk25) -- not
      just incidental anymore, a real prerequisite.

      DONE WHEN: compare compiles clean; gate green for compare and
      whatever else transitively needed it (okaySpark's own compile
      included); compare/BUGS.md entry updated to fixed with this
      commit's sha.
