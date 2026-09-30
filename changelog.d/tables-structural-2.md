## tables-structural-2 - DataFrames in a Tables program, and operators Catalyst can see

- Built on the operator's reasons, convenience and compatibility, after
  tables-structural measured the speed case at only 1.32x on the GTFS joins.
- `Structured` (okay-sql; specs/streams-seam.md, lane 5): `matching(w)` with
  a `Query.Where` over `Schema`-checked field names and `joinOn(r)(lf, rf)`
  with a `Query.Field` a side, a signature in the row beside `Tables`;
  `Structured.viaTables` answers it on any platform (localBulk, FlowBulk)
  through the opaque primitives.
- `SparkFrames` (okay-spark, which now depends on okay-sql): `load[A](df)`,
  `read[A](path, "parquet" | "csv" | "json")` pruned to A's fields,
  `frame[A](t)`; a DataFrame-born table meets `matching`/`joinOn` in Catalyst
  (the predicate compiled to a `Column`, a Parquet read shows it in
  `PushedFilters`), and after an opaque step the same operators answer
  through the RDD. `run(p)` runs a `Tables + Structured` program on Spark.
- Rows decode Row -> Json -> `Json.decode(Schema)`; a VARIANT/MAP column and
  a type deeper than `SparkSchema.MaxNesting` are refused by name.
  `Query.Where` became `Serializable`; the row codec lives in
  `object SparkFrames` so no executor closure ships the session.
- Tests: `TestStructured` (3, JVM + JS + Native), `TestSparkFrames` (4);
  docs/modules/okay-spark.md "DataFrames in a program". Not additive
  (build.sbt, `Query.Where`): gate `affected master staged`.
- Commits: 9e1b0dddf (Structured), 1d464c8f3 (SparkFrames).
