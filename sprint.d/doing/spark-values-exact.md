- [ ] spark-values-exact — SparkFrames' two named limits removed
      (operator, 2026-09-30: "Нужно это исправить"): (1) a `Long` above
      2^53 lost precision on the Row -> Json -> Schema road; (2) a VARIANT
      or MAP column was refused. `SparkValues`: a Schema-driven codec
      straight between Spark values and A — exact numbers (Long, BigInt as
      decimal(38,0)), a recursive type through the CBOR half of its
      `(cbor, json)` column, a VARIANT read typed through Spark's `Variant`
      (objects to products by key, longs and decimals exact), a MAP read as
      a list of its entries against the field's pair type; `frame`'s RDD
      side encoded by `Columns` on the executors (exact). Tests pin each.
