- [ ] lake-table-formats — Delta and Iceberg tables as the cluster
      engine's sources (operator, 2026-09-26). okay-lake reads Parquet
      objects under a prefix or a manifest; a Delta table's visible set is
      its `_delta_log`, an Iceberg table's its metadata and manifests.
      Needs: the current snapshot's data files resolved into the same
      row-group plan, without Hadoop (okay-delta's Kernel road is pinned to
      JDK 21). Gate: a Delta table written by another engine (pyarrow's
      deltalake, or okay-delta) planned and read, with a removed file NOT
      read.
