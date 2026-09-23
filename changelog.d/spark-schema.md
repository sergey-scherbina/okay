## spark-schema: ADTs as DataFrames — `SparkSchema` in okay-spark

The first lane of okay-scalus-spark (specs/scalus.md §4), generic and
not Cardano's: an okay `Schema[A]` folded into a Spark `StructType` and
external rows (`structOf`, `rows`, `dataFrame`, `column`). The
decisions Spark forces, each tested on a real SparkSession: a pure enum
is a string of the case NAME; a sum with payloads is `kind` plus one
nullable struct per case with fields (a field-less case has no branch —
Parquet refuses `struct<>`); a recursive type is
`struct<cbor: binary, json: variant>`, queried with `variant_get`;
`BigInt` is `decimal(38,0)`, refused past 38 digits. Old Parquet files
read under a schema whose enum gained a case (`mergeSchema`).

Found while building it: recursion detected by "my own name came back
through `ref`" misses MUTUAL recursion (B in A → B → A never sees its
own name); detection is now a reachability closure over a first fold,
pinned from either root. okay-spark now depends on okay-codec. Docs:
okay-spark guide "ADTs as DataFrames" with the gated snippet.
