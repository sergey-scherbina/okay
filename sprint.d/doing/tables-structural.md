- [ ] tables-structural — lane 5 of specs/streams-seam.md (operator:
      "Бери", 2026-09-30): the structural sub-language and `Bulk` over
      Spark DataFrames. MEASURE FIRST: the lane's premise "GTFS 18 s vs
      7 s through DataFrames" is stale — TestWroclawStages (bulk-rewrite,
      2026-09-09) found the 18 s was cache under Java serialization and
      the plan builds in 6.8 s — and no DataFrame version of the job
      exists in the repository. So: a hand-written DataFrame baseline of
      the three-joins stage beside SparkBulk and local, measured,
      recorded; then the structural nodes (where/joinOn/groupBy over
      named columns) with a local compile, and a DataFrame-backed instance
      that keeps a structural segment in Catalyst and drops to rows at an
      opaque function. Correct the stale 18/7 records wherever they are.
