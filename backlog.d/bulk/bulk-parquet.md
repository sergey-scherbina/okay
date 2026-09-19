- [ ] bulk-parquet — `Bulk.csv` is the only source; the taxi demo
      (TestTaxiAlgebra) still reads its parquet through Spark's API.
      A `source` per format, or `Bulk.read(Format)`, with the local
      instance reading parquet without Spark (okay-delta already
      carries a Delta Kernel road, specs/data.md).
