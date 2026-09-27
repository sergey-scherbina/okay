- [ ] foreign-measure-quiet-rerun — the TIMES of three Live instruments
      that foreign-measure-stateful-models-compression (2026-09-27)
      landed with bytes only: `MeasureArrowCompression` (okay-arrow),
      `MeasureRemote`'s implementation column (okay-cluster) and
      `MeasureForeignMapReduce`'s model and stateful lanes
      (okay-foreign-cluster). Two runs that day, at load 20–47 and 46–74
      on 14 cores with siblings benchmarking and gating, swung 2–4x per
      column and are on record as DISCARDED (history.d, the specs). Run
      the three on a quiet box — load under ~10 at the start AND the end,
      not `gate-retry`'s test, which reads the sbt count and free RAM and
      let a run start at load 24 — and replace the "not quoted" lines in
      docs/modules/okay-compress.md ("In place"), docs/modules/okay-arrow.md
      and specs/foreign-map-reduce.md ("The model and the stateful lanes")
      with numbers. Two leads to confirm or refute: aircompressor faster
      on the Arrow body's ZSTD (both runs, 3–7x, larger than
      `CompressBench`'s 1.9x decompress gap), and the stateful lane at or
      under the plain map's time.
