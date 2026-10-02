## tables-structural - the measure first: DataFrames buy 1.32x on the GTFS joins, not 2.6x; the stale claim corrected, the surface not built

- `MeasureGtfsFrames` (okay-spark, Live): the three joins of `Gtfs.departures`
  over the Wrocław GTFS (1,158,821 stop times against trips, routes and
  calendar, counted), three arms alternating, best of three — our seam on
  Spark (`SparkBulk`, the RDD level, columns pruned at the parser) 708 ms, a
  hand-written DataFrame 536 ms, `localBulk` in one JVM 803 ms; every arm
  the same count. Recorded in src/jmh/history.d (gtfs-frames-joins, 1.32).
- The premise of streams-seam lane 5, "18 s through the seam against 7 s
  through DataFrames", was stale: no DataFrame version of the job existed
  anywhere, and TestWroclawStages (2026-09-09) had shown the 18 s was
  `cache` under Java serialization. Corrected in docs/modules/okay-spark.md,
  specs/streams-seam.md (Results) and the arc's backlog item, which had
  copied it forward.
- Not built: the structural sub-language and `Bulk[DataFrame]`. 1.32x on
  CSV does not earn a second plan language. The measurement that could is
  filed as backlog `parquet-pushdown-measure`: Parquet with a selective
  predicate, where Catalyst's pushdown skips row groups and a closure
  cannot.
- Commits: bdcfdc183.
