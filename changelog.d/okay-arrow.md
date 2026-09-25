## okay-arrow - Arrow IPC behind one facade: ours on every platform, Apache Arrow Java optional

- New cross module okay-arrow (JVM, Scala.js, Native): `ArrowCodec`
  (`write`/`read` over a model of int64, float64, utf8, bool and null
  columns, nullable, with the schema's metadata), picked by a given.
- `OkayArrow`, the default: `okay.codec.ArrowIpc` moved here and
  optimised — the stream written once into an array of its exact size,
  UTF-8 encoded in place, bulk little-endian copies, the body read in
  place. 500k rows: write allocation 129 -> 17 MB (Arrow Java 64 MB) at
  the same speed; read to arrays 136 -> 70 MB and at least as fast as
  Arrow Java.
- `ApacheArrow` (JVM): the same facade over Arrow Java 19, plus
  `toRoot`/`fromRoot`; Arrow Java is an OPTIONAL dependency, and without
  it the first use is refused by name, saying what to add.
- okay-py's frames use `OkayArrow`; nothing reaches Arrow Java through it.
- Stage 0's verdict stands: in its own columns Arrow Java reads 15x
  faster (no object per row); the view design for that is deferred.
- Next, per the operator: more types, the typed layer through okay-codec,
  the file format, measured replacements of CBOR, and okay-compress
  (LZ4, ZSTD). specs/okay-arrow.md; docs/modules/okay-arrow.md.
