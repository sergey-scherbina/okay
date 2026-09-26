- [ ] duckdb-lake-reads — DuckDB over what okay writes (operator,
      2026-09-26: "записывай и делай"; filed as duckdb-sql-backend and
      renamed when the seam turned out to exist: DuckDB already reads
      Parquet and Delta through okay-sql's `Sql` seam via `JdbcSql`,
      specs/data.md, TestLake in okay-jdbc). What is missing: DuckDB as an
      oracle for okay-parquet (it reads ours, ours reads its), and a way to
      point DuckDB at EXACTLY a run's visible output — `Manifest.duckdb`, a
      `read_parquet([...])` of the manifest's objects, never a glob that
      would read a stray. Gate: both directions embedded; DuckDB over a
      Live MinIO reading an okay-lake run's output equal to the run.
