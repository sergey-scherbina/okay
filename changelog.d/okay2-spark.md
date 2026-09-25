## okay2-spark - Spark, native to okay2's own Scala 2.13

Operator request: port `okay-spark` to `okay2` so Spark integration
needs no Scala-version bridge at all — this build already compiles as
2.13, the same Scala Spark itself publishes for.

`SparkInterop.aggregate`/`aggregateByKey`/`toSpark` port unchanged: they
only ever needed `okay2.Aggregator`. `SparkBulk` — the `Bulk[Rows]`
seam, `Rows[A]` as a value class over `RDD[Any]` (Scala 2.13 has no
`opaque type`; the same simulation `Tables.Table[A]`'s own "opaque
`Int`" already uses) — reuses `okay2-stream`'s already-ported
`Bulk`/`Csv`/`Sort`/`Tables` unchanged too. `sort` answers `Sort`
NATIVELY (a real distributed `sortByKey` over the same `Tables.Heap`
`Tables.via` threads through `State`), which needed the one cast this
repository's own row dispatch always needs in Scala 2 for the identical
reason: scalac 2 does not refine a method's type parameter from a
constructor pattern (`Sort.By[a, k]`), so the correctly-typed value is
built first and the whole thing cast once at the boundary — the same
fix as `Bind(Inject(e), k)` instantiating its answer to `Any`.
`SparkSchema` (needs `Columns`/`Schema`/`Json`) waited on `okay2-codec`,
which landed while this was in flight — it DID need a follow-up
(`okay2-spark-columns`: `okay2-codec`'s own `Columns` and `SparkSchema`
itself, neither of which `okay2-codec`'s own lane had ported), not the
free ride this entry first assumed.

Two real defects, both found running against actual Spark, neither
guessable: `aggregateByKey`'s shuffle needs the exact `--add-opens` set
the Scala 3 build's own `sparkTestSettings` already names (Kryo
reflecting into `java.nio.HeapByteBuffer`, refused by JDK 17+'s module
system without them) — three of four SparkInterop tests never touch a
shuffle and passed with no flags at all, which is why only one failed.
And `csv`'s column-pruning closure called a private INSTANCE method,
which drags the whole `SparkBulk` (holding a `SparkSession`) into the
task and fails `Task not serializable` — moved to the companion object,
which holds no per-instance state.

10 tests, all against a real `local[2]` Spark session: the four
`SparkInterop` ones (an RDD aggregate, `zip`, `aggregateByKey`, the
Dataset side) and six `SparkBulk` ones (map/filter, join, aggregate,
cache, csv with column pruning, and the native sort — proven end to end
through `Tables.Heap`/`Ctx`, not just compiled).
