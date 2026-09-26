- [ ] duckdb-sql-backend — DuckDB behind okay-sql's `Sql` seam (operator,
      2026-09-26: "записывай и делай"). Analysts' SQL over the same lake
      files — Parquet, Delta, Iceberg on S3 — on one machine or inside one
      partition, and a third oracle for okay-parquet. Needs: the DuckDB
      JDBC driver as an OPTIONAL dependency, a `Sql` implementation over
      it, S3 credentials from the process's own lake registry. Gate: a
      query over Parquet objects okay-lake wrote answers what the engine
      computed; DuckDB reads okay-parquet's files and ours reads its.
