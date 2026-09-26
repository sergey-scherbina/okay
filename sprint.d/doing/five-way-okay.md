- [ ] five-way-okay — okay as a sixth runtime in Stanislav Shevchenko's
      five-way benchmark (github.com/stasimus/scala-effect-bench at
      82ac6f1: CE 3.7.1, Kyo 1.0.0-RC6, Loom, Ox 1.0.6, Gears 0.3.1).
      His harness, his workloads and JMH settings unchanged; okay added
      beside them by a patch kept here (his repo has no licence, so none
      of his code is copied in) and all six runtimes re-run on this box:
      bounded workers (8 x 4096, work 0 and 64), sequential spawn/join
      (1 000), runtime entry, TCP request batches (blocking and
      callback, parallelism 64). okay on its default Loom scheduler and
      on `own`. Numbers into docs/benchmarks.md. (2026-09-26, operator
      ask)
