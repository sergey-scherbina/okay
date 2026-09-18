- [ ] spark-4-2 — bump `spark-sql` 4.0.0 -> 4.2.0 and move
      `scala-reflect`/`legacyStdlib` from 2.13.16 to 2.13.18 with it
      (4.2.0 resolves 2.13.18; the two must stay a matched pair, and
      build.sbt says so beside the pins). The suite passed on 4.2.0
      during the migration, so this is a read of the release notes and
      a gate, not an investigation. Spark is still 2.13-only at 4.2.0,
      so nothing about the `for3Use2_13` arrangement changes.
      (was filed under "spark-4-2" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)
