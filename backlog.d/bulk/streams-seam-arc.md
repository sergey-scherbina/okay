- [ ] streams-seam-arc — ONE stream program, run on Spark, Flink or our
      engine; the joins and zips into the seam (operator ask, 2026-09-30).
      WHAT EXISTS, honestly: two seams. (a) `Bulk[D]` + `Tables`
      (specs/bulk.md): a BOUNDED program that names no platform —
      read/csv/map/flatMap/filter/join/cache/aggregate, a plan with
      rewrites, extension by a new signature in the row (`Sort`);
      instances `Chunks` (one JVM), Spark RDD, java List; Flink has NONE
      (bulk-flink). Its join is the hash equi-join alone. (b) `Flow`
      (specs/dataflow.md, okay-cluster): the UNBOUNDED/partitioned
      program — Src per partition, Local, Keyed, Windowed (event time),
      Exchange — with lake and Kafka sources; runs on OUR engine only.
      Nothing carries sort-merge, windowed join or zip on either seam;
      `Chunks.joinSorted`/`Source.joinWithin`/`zip` are local operators.
      THE ARC, four lanes, spec first (specs/streams-seam.md):
      1. `Bulk[Flow]` — our engine as a Bulk instance (join = Exchange by
         key on both sides, then the local hash join per partition;
         aggregate = Keyed with Finish.Auto), so every `Tables` program
         runs on the cluster engine unchanged, and one program has three
         backends the day this lands: Chunks, Spark, ours.
      2. `Streamed`, a new signature in the row beside `Tables`/`Sort`
         (the extension rule of specs/bulk.md): `joinSorted` (with the
         ordered-fact of [[join-strategy-auto]]), `joinWithin(within,
         lateness)(atL, atR)`, `windowed(size, slide, lateness)(key, at)
         (agg)`, `zip`. Each has the platform-free default through the
         local machines (`SortMerge`, `WindowJoin`, `Windows`,
         `Chunks.zip`) on `toChunks`, exactly as `Sort.viaTables` does,
         and a NATIVE answer where a platform has one: Spark sort-merge
         join and window aggregation on the RDD/Dataset, Structured
         Streaming's stream-stream join for `joinWithin`; Flink's
         interval join and windows; our `Flow.Windowed` and a
         `Flow.Join` node (Exchange by key, then `WindowJoin` per
         partition with the min-watermark seeded per partition, the
         `seed` rule of specs/dataflow.md). A program does not change
         between them — only the instance. ZIP IS NAMED WITH ITS LIMIT:
         positional zip of two distributed collections is defined only
         when both are partitioned alike (Spark's `RDD.zip` refuses
         otherwise); the seam offers `zip` for a bounded `D` and says
         so, and a stream zip is `Source.zip`'s local shape per
         partition.
      3. `Bulk[DataStream]` for Flink (bulk-flink), behind an optional
         dependency by the rule of AGENTS.md ("every dependency behind an
         abstraction, optional"), refused by name without the jar; the
         `Streamed` signatures answered natively there is where Flink
         earns its place (interval join, event-time windows).
      4. The docs page: one job — read two parquets or two topics, join
         by key within an interval, window, aggregate — written once and
         run three ways, with the numbers beside each (the Wrocław
         benchmark's job, docs/wroclaw-streams-benchmark.md, is the
         candidate: today its three versions are hand-written per
         platform).
      5. SPARK DATAFRAMES (operator's question, 2026-09-30) — not a
         `Bulk[DataFrame]` instance as such: the seam sits at the RDD
         level on purpose (specs/bulk.md, Out of scope), and on the
         GTFS CSV job it costs 1.32x (708 ms vs 536 ms for a hand
         DataFrame, MeasureGtfsFrames 2026-09-30; the "18 s vs 7 s" once
         quoted here was stale — cache under Java serialization), because
         a `map(f)`/`where(p)` carrying a Scala closure is
         OPAQUE to Catalyst, and a `Dataset.map(f)` on a typed Dataset
         is object mode (deserialise, call, serialise), often slower
         than the RDD. Catalyst pays only for what it can SEE. So the
         lane is a STRUCTURAL sub-language in the plan, typed by
         `Schema[A]`'s field names (the named-rows measurement in
         specs/bulk.md is the typing; okay-sql's `Query.Pred` —
         And/Or/Not over field comparisons — is the predicate language
         already written, engine-free): `select(fields)`, `where(pred)`,
         join ON a named key field, `groupBy(field)(agg)`, `sortBy(field)`
         as plan DATA beside the opaque `select(f)` that stays as the
         escape hatch. Then a `Bulk[DataFrame]` instance is honest: a
         structural plan compiles to column expressions end to end
         (`SparkSchema` already maps `Schema[A]` to a `StructType` and
         rows), Catalyst prunes, pushes, broadcasts and codegens, and
         [[join-strategy-auto]]'s `Auto` DELEGATES to Catalyst on that
         backend rather than second-guessing it; an opaque function in
         the middle drops that segment to object mode and `Plan.show`
         says where. Flink's Table API and our `Tables` rewrite read the
         same structural nodes (the `Columns(Read)` pruning rewrite is
         the first of them, landed). Measure the GTFS job three ways
         before and after — and FIRST a Parquet table with a selective
         predicate, where pushdown (row-group skipping) is what a closure
         can never get: 1.32x on CSV does not earn the surface alone.
      Refuted in advance: making `Flow` the one program and writing Spark
      and Flink interpreters for it — `Flow` is our engine's plan and
      carries our engine's decisions (Finish, Sequential, seeding); the
      platform-free program is the effect row (`Tables` + `Sort` +
      `Streamed`), and `Flow` is one of its targets, as an RDD is.
      Tests: the agreement law — every `Streamed` op answers the same
      multiset on Chunks, Spark (local mode), Flow (parallelism 4) and,
      when it lands, Flink (MiniCluster), on the same input; the
      windowed join's late-row count equal across engines with per-
      partition seeding. Literature: Beam's model (one pipeline, many
      runners — Akidau et al. 2015, "The Dataflow Model") is the prior
      art for exactly this, and the honest comparison for the docs page.
      (2026-09-30, operator ask)
