## flink-typed-road - FlinkBulk 3.5x faster on the one-job page, the cost found road by road

- The one-job page's job on Flink: 29.8 s -> 8.5 s, the same 1,158,821 rows.
  `ProbeFlinkRoads` (ignored by default) measured each change: a CSV read in a
  task instead of its rows shipped in the job graph 17.8 s, object reuse 16.2,
  a join on keyed state (`KeyedCoProcessFunction`, rows paired at the key's
  end-of-time timer) 16.3, STREAMING mode instead of BATCH 8.8.
- BATCH sorts every keyed input, each record through generic Kryo into the
  sorter — the largest single cost. STREAMING is FlinkBulk's default now; a
  join holds both sides in heap state, as the other instances' hash join holds
  one. `mode = RuntimeExecutionMode.BATCH` stays for sides larger than memory.
- The residual (17x one JVM) is generic Kryo per record per shuffle, the price
  of the seam asking no per-element evidence; said on docs/one-job-everywhere.md.
- Gate: `TestFlinkBulk` and `MeasureOneJobFlink` (Live), the compile of
  dependents. Commits: 3e6beae83.
