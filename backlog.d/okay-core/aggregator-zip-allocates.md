- [x] aggregator-zip-allocates — DONE (2026-09-10):
      `Aggregator.summary` is the flat count/sum/min/max accumulator,
      beside `Mean` and `Variance`, and it takes the Wrocław job's
      windowed lane from 810 ms to 580 (1.40x) — level with a
      hand-written MUTABLE cell, while keeping the value semantics
      `merge` needs. 83 B per `add` becomes 37.
