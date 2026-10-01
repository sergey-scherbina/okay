- [ ] flink-typed-road — FlinkBulk costs 59x one JVM on the one-job page
      (docs/one-job-everywhere.md: 29.8 s vs 0.5 s, the GTFS three joins):
      elements travel as `AnyRef` under generic Kryo type information and a
      join is a `coGroup` buffered in an end-of-stream window. Measure where
      it goes first (a profile of the MiniCluster run), then the known
      roads: rows from a `Schema` through `FlinkSchema` (typed
      serialization), Flink's own `join` or a keyed `CoProcessFunction` in
      BATCH mode instead of a windowed coGroup, `enableObjectReuse`. The
      page's row is the gate: the Flink number must move, or the lane has
      not earned its surface. (2026-10-01, filed by streams-seam-docs)
