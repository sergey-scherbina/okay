# okay-parquet

Parquet files read and written with nothing but okay on the classpath —
no Spark, no Hadoop (specs/parquet.md). A row group is an okay-arrow
`Table` ([okay-arrow](okay-arrow.md) has the column model), and the
reader and writer work a row group at a time, so a file of any size is
read and written in the memory of one group.

## Guide

**Reading.** A file is anything with a size and byte ranges — `ReadAt`:
an array (`ReadAt.of(bytes)`), a file, an S3 object by range GET. The
footer names the columns and the row groups; each group is read on its
own, and a projection reads only the named columns' chunks:

```scala
val in = ReadAt.of(bytes)
val f = OkayParquet.footer(in)
val g = OkayParquet.group(in, f, 2, Some(Set("name", "id")))
```

**Writing.** A writer hands bytes to a sink as each row group is
appended and writes the footer on `close()`; `write` is the same for a
whole table in memory, cut into groups:

```scala
val bytes = OkayParquet.write(t, groupRows = 1200, compress = compress)
```

Pages are compressed with Snappy (the default), ZSTD or not at all,
through [okay-compress](okay-compress.md)'s `Compression` in scope.

**Two implementations, one format** ([own-or-standard](../../specs/own-or-standard.md)).
Ours is the default everywhere; parquet-java is `import
okay.parquet.ParquetJava.given` on the JVM, over an optional dependency
(`org.apache.parquet:parquet-hadoop` with Hadoop's client API — no
Hadoop file system is touched). Each reads the other's files: the tests
write with one and read with the other, both ways, read what DuckDB
writes (its dictionaries, its DECIMALs) and have DuckDB read ours, and
read what pyarrow writes (data page v1 and v2, dictionary or plain, Snappy, ZSTD,
none) and have pyarrow read ours. `Parquets.byName("okay" |
"parquet-java")` picks one from a config value.

## What it reads and writes

| Parquet | `Column` | |
|---|---|---|
| BOOLEAN | `Bool` | both ways |
| INT32 (INT(8/16/32), signed or not) | `Ints(bits, signed, …)` | both ways |
| INT32 DATE | `Date32` | both ways |
| INT64, INT(64) | `Int64`, `Ints(64, …)` | both ways |
| INT64 TIMESTAMP millis/micros/nanos | `Timestamp(unit, zone)` — UTC-adjusted is `Some("UTC")` | both ways |
| FLOAT, DOUBLE | `Float32`, `Float64` | both ways |
| BYTE_ARRAY (STRING) / other | `Utf8` / `Binary` | both ways |
| FIXED_LEN_BYTE_ARRAY | `FixedBinary(width)` | both ways |
| INT96 (Spark's legacy timestamp) | `Timestamp(Nano, Some("UTC"))` | read |
| DECIMAL | `Decimal(precision, scale)` | read |
| a group | `Struct(fields)` | both ways |
| LIST (three-level; two-level and unannotated repeated read) | `ListOf(child)` | both ways |
| MAP | `ListOf(Struct(key, value))` | read |

Every node is written OPTIONAL — a list the three-level standard —
its values PLAIN, one data page per 64K rows. Read are data pages v1
and v2, dictionary pages, PLAIN, the dictionary encodings, RLE booleans,
DELTA_BINARY_PACKED, DELTA_LENGTH_BYTE_ARRAY, DELTA_BYTE_ARRAY and
BYTE_STREAM_SPLIT. Nested columns are Dremel's levels, assembled per
leaf; pyarrow's and DuckDB's nested files read equal, and they read ours.

## Gotchas

- `ParquetJava` is flat: a nested file is refused by name there; ours
  reads and writes both.
- Refused by name: GZIP, LZ4 or BROTLI pages. Spark's, DuckDB's,
  pyarrow's and parquet-java's defaults use none of them.
- A timestamp's zone other than UTC is written as UTC-adjusted and read
  back as `Some("UTC")`: Parquet records whether a timestamp is adjusted,
  not the zone.
- parquet-java's writer picks its own row groups (by size); ours cuts
  one per `append`.

## Literature

- Apache Parquet, *File Format* and *Encodings*
  (parquet.apache.org/docs/file-format) — the footer, pages, the
  RLE/bit-packed hybrid, the dictionary encodings.
- Melnik et al., *Dremel: Interactive Analysis of Web-Scale Datasets*,
  VLDB 2010 — the repetition and definition levels a flat schema reduces
  to one bit.
- Apache Thrift, *Compact Protocol* specification — the encoding of the
  footer and page headers.
