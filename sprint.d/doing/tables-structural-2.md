- [ ] tables-structural-2 — streams-seam lane 5 BUILT (operator,
      2026-09-30: "Удобство и совместимость это и есть достаточные
      причины"): `Structured` in okay-sql — `matching(Query.Where[A])`,
      `joinOn(r)(Field, Field)` over Schema-typed tables, `viaTables`
      default on every backend; okay-spark `SparkFrames` — a DataFrame
      enters a Tables program (`load`) and leaves it (`frame`), a read by
      Spark pruned to A's fields, and structural ops on a DataFrame-born
      table stay in Catalyst (Pred compiled to a Column), dropping to rows
      only at an opaque function. Agreement law across localBulk,
      FlowBulk and SparkBulk; Catalyst's plan read to prove the filter
      reached it.
