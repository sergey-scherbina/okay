## parquet-nested — nested Parquet columns, and the DELTA encodings

okay-parquet reads and writes okay-arrow's `Struct` and `ListOf` to any
depth `Column.MaxNesting` allows: Dremel's repetition and definition
levels, shredded per leaf on write (every node OPTIONAL, lists the
three-level standard) and assembled per leaf on read — a list's offsets
and a struct's validity from one leaf, each leaf's element ranges from
its own levels. Read too: the two-level and unannotated list forms, MAP
as a list of key/value structs, DELTA_BINARY_PACKED,
DELTA_LENGTH_BYTE_ARRAY, DELTA_BYTE_ARRAY and BYTE_STREAM_SPLIT.
pyarrow's and DuckDB's nested files read equal and they read ours;
`ParquetJava` stays flat and refuses a nested file by name.
