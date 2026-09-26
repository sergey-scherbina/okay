- [ ] compress-snappy — Snappy in the `Compression` facade (part of
      engine-object-store-io, 2026-09-26): parquet-java, Spark and DuckDB
      write Parquet pages Snappy-compressed by default, so a Parquet reader
      without Spark needs it. By the own-or-standard rule an implementation
      of ours lands behind the facade: `Compression.snappy`, ours as the
      default, aircompressor's behind `Aircompressor.given`, each reading
      the other's output.
