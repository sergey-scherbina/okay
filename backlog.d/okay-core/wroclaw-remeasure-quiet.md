- [x] wroclaw-remeasure-quiet — DONE (2026-09-10): §20's whole table
      is one run of `scripts/wroclaw-bench.sh 8 3 1` on a box under
      load 5, so the daggers are gone and the rows can be read against
      each other. It also PRICED the arithmetic rewrite: Flink +12%
      and a fifth less allocation, Spark's RDD lane +30%.
