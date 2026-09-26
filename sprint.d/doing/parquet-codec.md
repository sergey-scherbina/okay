- [ ] parquet-codec — Parquet read and written without Spark or Hadoop
      (part of engine-object-store-io, 2026-09-26; the reader bulk-parquet
      asks for). okay-parquet: our own codec over okay-arrow's `Table`
      (flat schemas; PLAIN and dictionary pages, v1 and v2; Snappy, ZSTD,
      uncompressed), row group by row group from a random-access reader,
      and a writer that holds one row group — behind a `ParquetCodec`
      facade with parquet-java behind an import, each reading the other's
      files (specs/own-or-standard.md). specs/parquet.md.
