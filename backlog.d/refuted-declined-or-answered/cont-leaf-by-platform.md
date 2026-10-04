- cont-leaf-by-platform — DECLINED 2026-10-04: one leaf for every platform
  stays. Measured, contAnswer's body `k(x + 1) + 1`, strict against lazy:
  | depth | JVM (JMH, ContDepthBenchmark) | Native (releaseFast, ProbeContDepthNative) |
  | 1 000 | 0.85x | 1.25x (worse) |
  | 100 000 | 0.29x | 0.77–0.89x |
  | 1 000 000 | 0.25x | 0.74–0.96x |
  (history.d cont-leaf-depth, cont-leaf-native.) Three reasons it is not
  worth a second code path:
  (1) THE LIBRARY GAINS NOTHING. A survey of every answer-using `Cont`
  body in main code found 10, and the macro picks the lazy leaf for none
  of them. Each calls `k` inside a lambda (`foldM` steps in `runChoice`,
  `runSeq`, `runExact`; Zoom's `(s1) => …`) or a by-name argument
  (`Put[LazyList]`'s `#::`), which the transform does not enter, so they
  are strict already. The choice would change only user bodies the macro
  can read.
  (2) Native does not share the JVM's win: shallow bodies get slower there.
  (3) Past a switch, a strict body's rest runs on another thread. A body
  the macro reads never did that before.
  Reopen if a real user workload shows deep, readable answer-using bodies
  on the JVM. The win there is 3–4x past 100 000 levels.
