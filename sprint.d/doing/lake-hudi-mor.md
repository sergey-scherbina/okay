- [ ] lake-hudi-mor — a Hudi MERGE-ON-READ table as the cluster engine's
      source (operator, 2026-09-26: "Бери"). A MOR file slice is a base
      Parquet file and log files (`.fileId_instant.log.N_token`) of
      `#HUDI#` blocks — Avro data blocks (upserts), delete blocks, command
      blocks (rollbacks) — merged over the base by record key. Needs: the
      log format read, a slice planned as one partition, records merged
      in completed-instant order with the table's merge mode (event-time
      ordering by the ordering field, or commit-time), deletes applied,
      rolled-back and uncommitted blocks skipped. Gate: a MOR table written
      by Hudi 1.2.1 (insert, upsert, delete, no compaction) read equal to
      Hudi's own read.
