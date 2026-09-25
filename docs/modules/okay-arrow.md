# okay-arrow

Apache Arrow IPC streams behind one facade, with two implementations
(specs/okay-arrow.md). A table is named columns of one length. The model
holds five kinds of column (int64, float64, text, bool, and all-null),
every one nullable, and the schema's metadata. That is exactly what
okay's wire carries to Python (`FrameFormat`, docs/python-and-r.md).

| | |
|---|---|
| `ArrowCodec` | the facade: `write(Table): Array[Byte]`, `read(Array[Byte]): Table`; `ArrowCodec.isStream` tells an Arrow stream from JSON or CBOR by its first four bytes |
| `OkayArrow` | OURS and the default: on the JVM, Scala.js and Scala Native, with no dependency |
| `ApacheArrow` | JVM only: the same facade over Apache Arrow Java 19, plus `toRoot`/`fromRoot` to and from a `VectorSchemaRoot`; Arrow Java is an OPTIONAL dependency you add |

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

Choose `OkayArrow` when the table is one of the five kinds of column and
the program should stay light and cross-platform. Its whole cost is this
module, it runs on Scala.js and Native, and it needs no JVM flags.
Choose `ApacheArrow` when the table lives in Arrow Java already, or when
the rest of Arrow is needed: nested types, dictionaries, the file
format, compression, Flight. `toRoot` hands the table to that world:

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

BENCH

## Literature

- Apache Arrow. *[Arrow columnar format](https://arrow.apache.org/docs/format/Columnar.html)*, and its IPC streaming format: what both implementations write and read.
- Daniel J. Abadi, Samuel R. Madden, Nabil Hachem. *[Column-stores vs. row-stores: how different are they really?](https://doi.org/10.1145/1376616.1376712)* SIGMOD 2008. Why a column is one buffer rather than a list of cells.
- Mark Raasveldt, Hannes Mühleisen. *[Don't hold my data hostage: a case for client protocol redesign.](https://doi.org/10.14778/3115404.3115408)* PVLDB 10(10), 2017. Moving a table cell by cell costs more than computing it.
