- [ ] foreign-measure-stateful-models-compression — the numbers the last
      lanes landed without (operator, 2026-09-27: "1 и 2", item 2):
      `MeasureForeignMapReduce` gains a stateful-stage lane and a model
      lane beside the plain map (1M rows, 4 partitions, fan and 3 workers,
      medians, load beside); and the library compression against ours
      where it actually runs — `Aircompressor.given` vs `Compression.Okay`
      inside Arrow bodies (okay-arrow, a Measure) and on `Remote`'s wire
      (`MeasureRemote`, an implementation column). Recorded in the specs.
