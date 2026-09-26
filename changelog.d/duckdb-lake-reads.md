## duckdb-lake-reads — DuckDB over what okay writes

DuckDB already read Parquet and Delta through okay-sql's `Sql` seam
(`JdbcSql`, specs/data.md), so the operator's "DuckDB backend" was
filed as duckdb-sql-backend and renamed when the seam turned out to
exist. What was missing, now built: DuckDB as a third oracle for
okay-parquet (it reads ours; ours reads its dictionary pages, its
codecs and its DECIMALs — `TestParquetDuckDb`, embedded), and
`Manifest.duckdb(root)` / `Manifest.of(lake, prefix)`: a
`read_parquet([...])` of exactly a run's visible objects, never a glob a
late writer could join (the glob mutant is caught). Live on MinIO with
DuckDB's httpfs (`TestLakeS3`). specs/data.md records that the consumer
it staged okay-parquet for has arrived.
