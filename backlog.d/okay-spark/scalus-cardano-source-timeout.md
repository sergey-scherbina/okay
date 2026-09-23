- [ ] scalus-cardano-source-timeout — `okay.scalus.spark.TestCardanoSource`
      "batch: the outputs table is CardanoTables' outputs, row for row"
      timed out at munit's 30 s in a full gate on 2026-09-23 16:20 (load
      averages 41/48 rising to 76 minutes later — siblings' builds), and
      passed 4/4 run alone a minute after. A Spark batch under that load,
      not a wrong answer: measure its time on a quiet box, then either
      size its `munitTimeout` from that with a margin, or tag it `Live`
      if it cannot be bounded (the no-flaky-tests policy). Found by
      ts-scalajs, which does not touch okay-scalus-spark.
