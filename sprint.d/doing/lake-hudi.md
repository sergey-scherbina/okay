- [ ] lake-hudi — a Hudi table as the cluster engine's source (operator,
      2026-09-26: "Худи добавим тоже в озера данных?"). A COPY-ON-WRITE
      table's snapshot is its `.hoodie` timeline's completed instants and
      the latest file slice per file group — Parquet base files, which
      become okay-lake's row-group plan, as Delta's do (stage 18).
      MERGE-ON-READ needs its Avro log blocks merged over the base files:
      refused by name until an Avro reader exists (lake-iceberg builds one)
      and the merge is its own stage. Hudi 1.x's LSM timeline is a second
      format to check. Gate: a COW table written by Hudi itself (Spark with
      the hudi bundle — hudi-rs's python binding only reads) with inserts,
      an update that replaces a file slice and a clean, planned and read
      equal to what Hudi reads; an older slice NOT read.
