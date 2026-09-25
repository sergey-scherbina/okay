## okay-arrow-typed - any Schema[A] to Arrow and back, through okay-codec

okay-arrow stage 5 (specs/okay-arrow.md): `OkayArrow.encode(rows)` and
`OkayArrow.decode[A](bytes)` (and the same on `ApacheArrow`), with
`Rows.table`/`Rows.rows` underneath.

- The write is okay-codec's `Columns` — the tabular reading of a `Schema`
  the Spark, DuckDB and Delta encoders already share — translated to the
  model: nested products as structs, sequences as lists, `Option` as null,
  enums by case name, sums as `kind` plus branches, `BigInt` as
  decimal(38, 0), recursive types as CBOR beside JSON.
- The read is a fold over the same `Schema` making those decisions
  backwards; a misfit is a `Left` naming the row and the column.
- Runs on the JVM, Scala.js and Native; pyarrow reads typed rows as
  ordinary Arrow. okay-arrow now depends on okay-codec.
