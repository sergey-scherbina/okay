## scalus-cardano-source-timeout — TestCardanoSource's first test no longer pays the setup

- `okay.scalus.spark.TestCardanoSource`: the Spark session and the
  replayed reference tables are built in `beforeAll`, not inside the
  first test's 30 s budget; the suite's budget is 120 s, sized from a
  measurement (session 1.2 s, first query 1.7–2.1 s, the rest under
  0.9 s at load 22) — ~60x the slowest test. The first query itself
  (Spark's code generation) is most of the first test's time, so the
  sized budget, not the move, is what covers a loaded box; the suite
  stays in the default gate.
