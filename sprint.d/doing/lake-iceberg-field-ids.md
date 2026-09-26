- [ ] lake-iceberg-field-ids — an Iceberg table read by FIELD ID, not by
      name (found by lake-iceberg, 2026-09-26). Iceberg names a column by
      a numeric id carried in each Parquet file's schema; a column renamed
      after a file was written keeps its id and changes its name, and a
      column added later is absent from older files. Reading by name
      missed the first and failed on the second. Needs: okay-parquet's
      `Footer` to carry the top-level columns' field ids (ours and
      parquet-java's), and `IcebergSource` to rename each file's columns to
      the current schema's names by id and add the missing ones as nulls
      typed by the schema. Gate: pyiceberg renames a column and adds one
      between appends; every row reads with the current names, old rows
      null in the new column, equal to pyiceberg's read.
