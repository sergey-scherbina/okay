## okay2-spark-columns - Columns and SparkSchema, native to okay2

Follow-up to `okay2-spark`: with `okay2-codec` landed (`Schema`/`Json`),
the two remaining pieces of okay's Spark port — `okay-codec/Columns.scala`
(the tabular reading of a `Schema`, engine-free) and `okay-spark/SparkSchema.scala`
(Columns onto Spark's own types) — port to `okay2-codec.Columns` and
`okay2-spark.SparkSchema`, completing the operator's "full port" of
`okay-spark` to `okay2`.

One real, documented narrowing: `okay2-codec` has not ported `Cbor`
(`okay2-codec-dialects`, "on demand"), and the Scala 3 core's
`Columns.recursive` writes a RECURSIVE type as `struct<cbor, json>`.
Here it is `struct<json>` alone — still a usable, queryable
representation (Spark's `variant_get` reads through it exactly the
same), just missing the binary column, stated as a limitation in
`Columns.scala`'s own doc comment rather than silently dropped.

Two Scala-2-only traps, neither guessable from the Scala 3 source,
both now in memory (`case-object-does-not-widen-in-scala2`,
`implicit-paren-list-eats-next-call`):
- a Scala 2 `case object` does not widen to its sealed parent under
  generic inference the way a Scala 3 `enum` case does — `Columns.row(Tag.Spend)`
  silently derived a schema for the SINGLETON type instead of reusing
  `Tag`'s sum, and returned the field-less-product placeholder `true`
  where `"Spend"` was wanted. Caught only by the port's own test.
- a plain paren list right after a method whose only parameter list is
  implicit fills THAT list in Scala 2 (`structOf[A]("tag")` tried to
  pass `"tag"` as `Schema[A]`); Scala 3's `using` needs the keyword to
  do the same, so the identical-looking source has no such ambiguity
  there.

13 tests: 5 in `TestColumns` (types, values, mutual recursion by
reachability, the 38-digit `BigInt` refusal, a non-product root), 8 in
`TestSparkSchema` against a real `local[2]` Spark session (enum as
string, sum as kind+branches, Option/Vector/BigInt encoding, the
`json`-only recursive struct queried with `variant_get`, mutual
recursion, a Parquet round-trip, schema evolution across two Parquet
directories, the same `BigInt` refusal on the Spark side). Full okay2
suite: 2284 tests green.
