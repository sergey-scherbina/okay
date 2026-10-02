## spark-values-exact - DataFrame rows decode exactly: big longs, BigInt, recursive types, VARIANT and MAP

- `SparkValues` (okay-spark) replaces SparkFrames' Row -> Json ->
  `Json.decode` road with a Schema-driven codec straight over Spark's
  values: a `Long` above 2^53 and a 38-digit `BigInt` arrive exactly; a
  recursive type is read from the CBOR half of its `(cbor, json)` column;
  a VARIANT column is read typed through Spark's `Variant` (objects into
  products by key, longs and decimals exact, any value into a `String`
  field as JSON); a MAP column is read as its entries into a sequence of
  (key, value) products. Both limits tables-structural-2 had named are gone.
- `frame` of an RDD-side table encodes by `Columns` on the executors
  (`mapPartitions`), exact where the Json road was not.
- SparkFrames' element casts go through one documented `elem`.
- Tests: `TestSparkFrames` +4 (longs and BigInt both ways, a 40-level
  recursive tree both ways, a MAP column, a VARIANT column). Gate
  `affected master staged`.
- Commits: 764fcddcf.
