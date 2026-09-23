## columns-neutral: the tabular reading of a Schema, with no engine in it

Operator: the Cardano tables must work in okay-watch, which does not want
Spark. So the decisions spark-schema made — a pure enum as the case NAME,
a sum with payloads as `kind` + one nullable struct per case with fields,
recursion (by reachability, mutual included) as `cbor` + json, `BigInt`
as `decimal(38,0)` — move into okay-codec as `Columns`: column types
(`Int32`, `Int64`, `Text`, `Binary`, `Decimal`, `Json`, `Arr`, `Struct`)
and rows of plain Scala values, cross-built. okay-spark's `SparkSchema`
is now a translation of that (a `Json` column is a VARIANT) and decides
nothing; every Spark test passed unchanged over it. `TestColumns` checks
the same decisions with no Spark on the classpath. specs/scalus.md
records the second half of the decision: §5's tables become typed Scala
rows in okay-scalus, not SQL views inside Spark.
