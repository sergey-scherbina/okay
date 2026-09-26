## five-way-okay - okay in someone else's harness: the five-way Scala runtime benchmark

- okay is a sixth runtime in Stanislav Shevchenko's scala-effect-bench
  (CE, Kyo, Loom, Ox, Gears), added by `compare/five-way/apply.py` to a
  local clone. His workloads and JMH settings are unchanged, and
  nothing was pushed to his repository. okay's backend mirrors his CE
  code; its answers are validated against his Loom backend, and his own
  validations pass under Scala 3.9.
- Measured on this box with three okay schedulers. Sequential
  spawn/join: okay on `own` 14 486 ops/s against Kyo's 6 371, the case
  his write-up gives to Kyo; okay's default reads Loom's 472. Bounded
  workers at work 64: the default ties the direct-style runtimes. TCP:
  the default matches Loom; `own` and `adaptive` lead the callback
  transport.
- Found: `Schedulers.adaptive` collapses on blocking TCP (5.4 against
  CE's 121), the same way Kyo does. Filed as
  `adaptive-short-blocking-calls`.
- docs/benchmarks.md §4a has both tables, allocation included; the raw
  JMH files are in compare/five-way/results-2026-09-26.
