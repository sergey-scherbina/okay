- [ ] lake-delta — a Delta table as the cluster engine's source (operator,
      2026-09-26; lake-table-formats split in two, lake-iceberg filed).
      The snapshot is read from `_delta_log` without Hadoop — the last
      checkpoint (Parquet, nested: okay-parquet since parquet-nested) and
      the JSON commits after it — and its data files become okay-lake's
      row-group plan, partition values as columns. Reader features this
      reader does not honour (deletion vectors, column mapping, v2
      checkpoints) are refused by name. Gate: a table written by
      delta-rs (pyarrow's `deltalake`) with appends, a DELETE that removes
      files, a checkpoint and commits after it — planned, read equal to
      what `deltalake` reads, a removed file NOT read.
