## spark-deep-values - no nesting limit on our side of Spark: the walks trampolined, Spark's own limits measured

- `SparkSchema` (type and value conversion) and `SparkValues` (row
  decoding) walk by `!.tailcall` in a program `! Pure`, run by `!.run`:
  stack-safe at any depth, and the refusal past 64 levels — Arrow's limit,
  in a module that is not Arrow — is gone. `SparkSchema.MaxNesting` removed.
- MEASURED (`ProbeSparkDepth`, ignored by default, Spark 4.2.0): Spark
  builds and collects a 512-level struct/array type; its first refusal is
  Jackson's JSON nesting limit (1000) — on a Parquet write's schema JSON at
  512 levels, on `parse_json` at 1024 — a named `StreamConstraintsException`
  of Spark's own, which is now the answer past what Spark takes.
- Tests: `TestSparkDepth` (a 5000-level type and value on a 256 KB stack),
  `TestSparkFrames` +2 (a 900-level VARIANT, a 300-level array value on a
  small stack). Gate `affected master staged`.
- Commits: b3a34b8d8.
