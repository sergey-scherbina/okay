## lake-iceberg-field-ids — Iceberg columns matched by field id

okay-parquet's `Footer.fieldIds` carries each top-level column's field
id (ours and parquet-java's read the same ones). `IcebergSource.plan`
reads the current schema and, per data file, renames its columns to the
current names by id and adds a column the file lacks as nulls typed by
the schema — a renamed column no longer disappears, an added one no
longer fails the read. `TestIceberg`: pyiceberg renames `city` to `town`
and adds `score` between appends; every row reads with the current names
equal to pyiceberg (the mutant that skips the rename is caught);
`TestParquetPyArrow`: pyarrow's field ids read the same by both codecs.
