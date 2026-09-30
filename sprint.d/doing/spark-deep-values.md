- [ ] spark-deep-values — the MaxNesting 64 refusal in SparkValues /
      SparkSchema lifted (operator, 2026-09-30: "Делай"): our walks made
      stack-safe (trampolined), a VARIANT value decoded at any depth, and
      the struct/array bound set to the depth Spark itself survives —
      MEASURED by a probe (createDataFrame, collect, Parquet write/read at
      depth N) — instead of Arrow's 64, refused by name just before it.
