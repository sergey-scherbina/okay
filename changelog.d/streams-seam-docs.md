## streams-seam-docs - one job, written once, run on five platforms, with the cost of each

- docs/one-job-everywhere.md: `okay.wroclaw.OneJob.departures` (compare) — the
  Wrocław GTFS three joins and a count as one `Tables` program — run unchanged
  on `Bulk.local` (508 ms), `BulkParallel(4)` (365), `FlowBulk(4)` (558), Spark
  local[4] (1,647) and Flink MiniCluster 4 (29,802); 1,158,821 rows everywhere;
  best of 3 on a shared box. Flink's cost is the generic-Kryo seam and a
  windowed coGroup, said on the page with the typed road that would fix it.
- Found by the page: a join fed by another join failed on our engine. `FlowBulk`
  now passes a keyed side through a lazy materialised boundary;
  `TestFlowBulk` pins a chain of joins at 1 and 4 partitions.
- Measurements `MeasureOneJob` / `MeasureOneJobSpark` / `MeasureOneJobFlink`
  (Live); src/jmh/history.d one-job-everywhere. A stale `import okay.given` in
  TestFlinkBulk removed (a warm gate had hidden it).
- Commits: 737509a16.
