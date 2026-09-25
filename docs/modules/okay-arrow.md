# okay-arrow

Apache Arrow IPC streams behind one facade, with two implementations
(specs/okay-arrow.md). A table is named columns of one length, with the
schema's metadata. The model is okay's columnar format, and every column
in it is nullable:

| Arrow type | `Column` |
|---|---|
| int8/16/32 and uint8/16/32/64 | `Ints(bits, signed, …)` |
| int64 | `Int64` |
| float16 (read), float32, float64 | `Float32`, `Float64` |
| bool, null | `Bool`, `Nulls` |
| utf8, large_utf8 (read) | `Utf8` |
| binary, large_binary (read), fixed_size_binary | `Binary`, `FixedBinary` |
| decimal128 | `Decimal(precision, scale, …)`: the unscaled value |
| date32, date64 | `Date32` (days), `Date64` (milliseconds) |
| timestamp, with unit and zone | `Timestamp(unit, zone, …)` |
| duration | `Duration(unit, …)` |
| list, large_list (read) | `ListOf(offsets, child, …)` |
| struct | `Struct(fields, …)` |

A compressed body (LZ4_FRAME or ZSTD, as pyarrow writes with
`compression=`) is read, and `OkayArrow.write(t, Some(codec))` writes one,
through [okay-compress](okay-compress.md). A dictionary-encoded column is
read as its values. What the model does
not hold (decimal256, maps, unions, run-end encoding, views, intervals,
times of day) is refused by name. The wire to Python uses five of these
columns (`FrameFormat`, docs/python-and-r.md).

| | |
|---|---|
| `ArrowCodec` | the facade: `write(Table): Array[Byte]`, `read(Array[Byte]): Table`; `ArrowCodec.isStream` tells an Arrow stream from JSON or CBOR by its first four bytes |
| `OkayArrow` | OURS and the default: every type above, on the JVM, Scala.js and Scala Native, with no dependency |
| `Rows`, `encode`/`decode` | typed rows of any `Schema[A]` to a table, and back, through either implementation |
| `ApacheArrow` | JVM only: the same facade over Apache Arrow Java 19, plus `toRoot`/`fromRoot` to and from a `VectorSchemaRoot`; Arrow Java is an OPTIONAL dependency you add |

## Files

`writeFile` writes an Arrow IPC FILE: the stream, then a footer that names
each record batch's place. `fileBatches` and `readFileBatch` find one batch
from that footer, without reading the batches before it. This is the
format `pyarrow.ipc.open_file`, DuckDB and Arrow Java read, and both
implementations write and read it:

```scala
      val f = OkayArrow.writeFile(Tables.everything, codec)
      assertEquals(Tables.same(Tables.everything, OkayArrow.readFileBatch(f, 0)), None, codec.toString)
```

## Typed rows

Any datatype with an okay-codec `Schema` goes to Arrow and back through
either implementation, one column per field:

```scala
  final case class Line(product: String, n: Int) derives Schema
```

```scala
    val back = OkayArrow.decode[Order](OkayArrow.encode(orders))
```

The tabular reading of a type is okay-codec's `Columns`, the same one
the Spark, DuckDB and Delta encoders use:
- a nested case class becomes a struct;
- a `List` or `Vector` becomes a list, and an `Option` a null;
- an enum of field-less cases becomes its case NAME as text;
- a sum with payloads becomes a struct of `kind` and one nullable branch
  per case;
- a `BigInt` becomes decimal(38, 0);
- a recursive type becomes its CBOR, beside its JSON.

pyarrow reads the result as ordinary Arrow: `lines` is a list of
structs, and the sum is `{"kind": "Circle", "Circle": {"r": 1.5},
"Square": null}`. Reading back folds the same `Schema`. A table that does
not fit is a `Left` that names the row and the column:

```scala
      Left("row 1: column 'sku': null where the schema has no Option around a String"))
```

## Which one

With no import, the given is ours:

```scala
    assertEquals(summon[ArrowCodec].name, "okay")
```

An import picks Arrow Java:

```scala
    import ApacheArrow.given
    assertEquals(summon[ArrowCodec].name, "apache")
```

The two write the same format, and each reads the other's streams
(pyarrow reads both as well):

```scala
    same(t, ApacheArrow.read(OkayArrow.write(t)))
    same(t, OkayArrow.read(ApacheArrow.write(t)))
```

Choose `OkayArrow` when the table's columns are in the model and the
program should stay light and cross-platform. Its whole cost is this
module, it runs on Scala.js and Native, and it needs no JVM flags.
Choose `ApacheArrow` when the table lives in Arrow Java already, or when
the rest of Arrow is needed: nested types, dictionaries, the file
format, compression, maps and unions, Flight. `toRoot` hands the table to that world:

```scala
      val root = ApacheArrow.toRoot(t, alloc)
```

## The optional dependency

okay-arrow declares Arrow Java as `optional` (Maven's `<optional>`), so
nothing that depends on okay-arrow gets it, okay-py included. A program
that imports `ApacheArrow` adds it itself, and gives the JVM the flags
Arrow's off-heap memory needs:

```sbt
libraryDependencies ++= Seq(
  "org.apache.arrow" % "arrow-vector" % "19.0.0",
  "org.apache.arrow" % "arrow-memory-unsafe" % "19.0.0")
javaOptions ++= Seq("--add-opens=java.base/java.nio=ALL-UNNAMED",
  "--sun-misc-unsafe-memory-access=allow")   // the second on JDK 24+
```

Without it, the first use is refused by name, and the message says what
to add, or to use `OkayArrow` instead.

## Measured

`ArrowIpcBench` (JMH, a box at load ~240, so the times are wide and the
allocation per operation is the firm number):

| 500k rows (float64, int64, text) | `OkayArrow` before | `OkayArrow` | Arrow Java 19 |
|---|---|---|---|
| write from arrays | 129 MB, 16–39 ms | 17 MB, 15–18 ms | 64 MB, 15–19 ms |
| read into arrays | 136 MB, ~44 ms | 70 MB, ~42 ms | 105 MB, 46–157 ms |
| read into its own columns | 136 MB, 30–42 ms | 70 MB, 14–19 ms | 14 KB, ~1 ms |
| through the facade, both ways to the model | | the rows above | write 65 MB, 14–22 ms; read 122 MB, 64–146 ms |

Through the model, ours writes with a quarter of the allocation and
reads at least as fast. Arrow Java's own columns win the last row by far:
it loads buffers as they are and makes no object per row, where the
model holds a `String` per row. Reading as views over the bytes is the
spec's deferred item for that.

## Literature

- Apache Arrow. *[Arrow columnar format](https://arrow.apache.org/docs/format/Columnar.html)*, and its IPC streaming format: what both implementations write and read.
- Daniel J. Abadi, Samuel R. Madden, Nabil Hachem. *[Column-stores vs. row-stores: how different are they really?](https://doi.org/10.1145/1376616.1376712)* SIGMOD 2008. Why a column is one buffer rather than a list of cells.
- Mark Raasveldt, Hannes Mühleisen. *[Don't hold my data hostage: a case for client protocol redesign.](https://doi.org/10.14778/3115404.3115408)* PVLDB 10(10), 2017. Moving a table cell by cell costs more than computing it.
