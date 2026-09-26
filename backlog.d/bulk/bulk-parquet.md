- [ ] bulk-parquet — `Bulk.csv` is the only source; the taxi demo
      (TestTaxiAlgebra) still reads its parquet through Spark's API.
      A `source` per format, or `Bulk.read(Format)`, with the local
      instance reading parquet without Spark. THE READER EXISTS since
      parquet-codec (2026-09-26): okay-parquet's `ParquetCodec` reads a
      file row group by row group into okay-arrow `Table`s, no Spark or
      Hadoop — what is left is the `Bulk` source over it and moving the
      taxi demo off Spark's reader.
