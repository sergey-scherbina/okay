- [x] aggregator-zip-flat-general — DONE (2026-09-10):
      `OfLong.zipLong` keeps the specialization through the pair — a
      flat `Longs2` accumulator, both sides stepped by `addLong` — and
      `AggregatorZipBenchmark` prices it in BYTES, which a loaded box
      cannot blur: 90.5 B/element for `count zip sumLong` against 50.8
      for `count zipLong sumLong`, error ±0.4 B/op. A new name rather
      than an overload of `zip`, so no inferred accumulator type
      changes under a caller who did not ask.
