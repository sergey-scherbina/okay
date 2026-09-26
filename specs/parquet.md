# okay-parquet: Parquet without Spark or Hadoop

## Why

The operator's case (2026-09-26): risk and fraud models in R and Python
reading and writing large data in object storage, run by okay's cluster
engine. The data is Parquet, and until this module the only Parquet
road in the repository was Spark's or Delta Kernel's — both over Hadoop,
which meets JEP 486 on JDK 24+ and pins okay-delta's tests to JDK 21. A
cluster worker must read a row group of a Parquet object with nothing
but okay on its classpath (engine-object-store-io; bulk-parquet asks for
the same reader).

## The design

- **The data model is okay-arrow's `Table`** — named `Column`s with
  validity. Parquet is a columnar file of the same columns; a row group
  is a `Table`. No second in-memory model.
- **Random access, one row group at a time.** `ReadAt` is two methods
  (`size`, `read(offset, len)`); a file on disk, an S3 object by range
  GET and a byte array all are one. The reader fetches the footer, then
  each row group's column chunks as byte ranges — memory is one row
  group's chunks, never the file.
- **The writer holds one row group.** `ParquetCodec.writer(out)` hands
  bytes to a sink as each row group is appended, and the footer at
  `close()`; the caller decides the row group's size by the tables it
  appends.
- **The facade (specs/own-or-standard.md)**: `ParquetCodec`, ours
  (`OkayParquet`) the default given on every platform, parquet-java
  (`ParquetJava`, JVM, over an OPTIONAL dependency) behind an import,
  refused by name without its jar, each reading the other's files.

## Scope

- Schemas of any nesting since stage 2 (structs, lists, maps as lists
  of key/value structs); stage 1 was flat.
- Types, both ways: BOOLEAN, INT32 (with INT(8/16/32, signed or not) and
  DATE), INT64 (with INT(64) and TIMESTAMP millis/micros/nanos), FLOAT,
  DOUBLE, BYTE_ARRAY (STRING → `Utf8`, otherwise `Binary`),
  FIXED_LEN_BYTE_ARRAY. Read only: INT96 (Spark's legacy timestamp → a
  nanosecond `Timestamp`), DECIMAL over INT32/INT64/FIXED (→ `Decimal`).
- Pages: data page v1 and v2, dictionary pages; encodings PLAIN,
  PLAIN_DICTIONARY/RLE_DICTIONARY, the RLE/bit-packed hybrid for
  levels, and RLE for booleans (pyarrow's data page v2 writes them so). The DELTA_* and BYTE_STREAM_SPLIT encodings are refused by
  name (parquet-java's default v1 writer, Spark's and DuckDB's defaults
  use none of them).
- Compression: UNCOMPRESSED, SNAPPY, ZSTD through okay-compress's
  `Compression` in scope; GZIP, LZ4, BROTLI refused by name.
- Written: PLAIN values, every column OPTIONAL, one data page v1 per
  column per row group (at most `pageRows` rows each), Snappy by
  default.

## Behavior

- [x] a table of every writable type, nulls included, round-trips
      through our writer and reader, row group by row group
- [x] ours writes, parquet-java reads the same rows; parquet-java
      writes (dictionary pages, Snappy, its defaults), ours reads the
      same rows (JVM, `ParquetJava`)
- [x] the reader holds one row group: a file of many row groups is read
      group by group from a `ReadAt` that counts the bytes it hands out,
      and no read exceeds one group's chunks plus the footer
- [x] with no import the codec is ours; `ParquetJava.given` picks the
      library; without its jar the first use is refused by name
- [x] a nested schema, an unsupported encoding or codec, a file cut
      short or with a bad magic is refused by name
- [x] by name: `Parquets.byName("okay" | "parquet-java")`
- [x] pyarrow as a second oracle: its files (data page v1 and v2,
      dictionary or plain, Snappy/ZSTD/none) read by ours, ours read by
      it (TestParquetPyArrow; skips without a python with pyarrow)

## Stage 2 — nested columns (parquet-nested, 2026-09-26)

- [x] structs, lists and maps both ways: okay-arrow's `Struct` and
      `ListOf` shredded into repetition and definition levels (Dremel) on
      write, assembled from them on read, to any depth `Column.MaxNesting`
      allows; a map reads as a list of `key`/`value` structs
- [x] the list forms other writers use are read: the three-level
      standard, the two-level legacy (a repeated primitive, a repeated
      group named `array` or `<name>_tuple`), and an unannotated repeated
      field
- [x] pyarrow's and DuckDB's nested files read equal to what they wrote;
      ours read by both equal to what we wrote
- [x] DELTA_BINARY_PACKED, DELTA_LENGTH_BYTE_ARRAY, DELTA_BYTE_ARRAY and
      BYTE_STREAM_SPLIT read (pyarrow writes each on request)
- [x] `ParquetJava` stays flat and refuses a nested file by name

## Decisions

- **Ours, over Arrow's Table, rather than parquet-java alone.**
  parquet-java reaches Hadoop's `Configuration` from its readers and
  writers, and okay-delta already had to pin its tests to JDK 21 for
  Hadoop's JEP 486 wall; a worker that must run on the JDK the rest of
  okay runs on cannot carry that. parquet-java stays as the standard
  implementation behind an import, on the rule's terms, and is how the
  format is checked.
- **A flat schema first, said.** The operator's data (model features
  and scores) is flat; nested columns (Dremel levels over lists and
  structs) are a stage of their own, refused by name until then.

## Results (parquet-codec, 2026-09-26)

**parquet-java ran on JDK 26 with no Hadoop file system.** The fear in
the first Decision was its readers and writers reaching Hadoop; through
its own `InputFile`/`OutputFile` and a `PlainParquetConfiguration` they
do not, and the standard implementation's tests run on the JDK the rest
of okay runs on. The Decision stands for the dependency's weight (the
Hadoop client API is 20 MB), not for the JDK.

**Every interop test passed the first time it ran, and the one that did
not was pyarrow's**: its data page v2 writes booleans RLE-encoded, which
the reader had refused by name. Supported now; the refusal worked as
designed, naming the column and the encoding.

**The dictionary road is checked by breaking it**: an index read one
off turns both parquet-java-writes tests red (a mutant), so ours is not
passing on PLAIN pages alone.
- **Nested columns are assembled per leaf, not record by record**
  (stage 2). Dremel's record assembly walks every leaf of a record in
  step; the columnar form — which pyarrow and parquet-java's vectorised
  readers use — decides a list's offsets and a struct's validity from
  ONE leaf's levels and each leaf's own element ranges, so a column is
  built in one pass per leaf and never as a tree of objects per row.

## Results (parquet-nested, 2026-09-26)

**Columnar assembly held on the first run against every writer**: our
own round trip (lists of strings with null lists, empty lists and null
elements; structs with null fields; lists of structs; lists of lists),
pyarrow's nested files at data page v1 and v2 with and without
dictionaries (maps included), and DuckDB's. The mutant that never
splits a list at its repetition level turns the round trip red.

**The DELTA encodings read what pyarrow writes on request** — sorted
and scattered INT64 and INT32 (DELTA_BINARY_PACKED), prefix-sharing
strings (DELTA_BYTE_ARRAY), byte strings by length
(DELTA_LENGTH_BYTE_ARRAY), doubles split by byte (BYTE_STREAM_SPLIT).

**The test was wrong once and the tool was right**: hand-computed
expectations of which tags row 3 holds disagreed with pyarrow reading
our file; the file was right, and the test now derives what pyarrow
should print from the values it wrote.
