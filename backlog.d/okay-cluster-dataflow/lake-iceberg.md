- [ ] lake-iceberg — an Iceberg table as the cluster engine's source
      (operator, 2026-09-26; split from lake-table-formats). The current
      snapshot is its metadata JSON's `current-snapshot-id`, whose manifest
      list and manifests are AVRO object container files — okay has no
      Avro reader yet (the first piece of work). Then the data files
      (Parquet) become okay-lake's row-group plan, delete files refused by
      name. Gate: a table written by pyiceberg (appends, an overwrite)
      planned and read equal to what pyiceberg reads.
