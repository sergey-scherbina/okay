## perf-records-stale - six performance records brought to the measurements they cite

Docs-only. Every edit is dated and names the lane whose measurement it
takes. Nothing was re-measured and no prose was rewritten.

- docs/benchmarks.md: the Reader row (short version and §2) reads 60.6
  (effect-op-cost), not the older 79. §22 adds the same-series ratio:
  Free 13.1 against staged 7.77, 1.69x, beside direct-staged's 2.24x.
  §4 loses the "8 µs of bookkeeping" its next paragraph refuted; it now
  says ~27 ns a fork/join. §6's elementwise 122 and §6b's elementwise
  lanes are marked as predating source-merge-via-ready. §2's 262-byte
  relay loop is dated beside the 266 read at resume-inline-budget-guard.
- specs/interpreter-optimization.md is marked HISTORY at the top: since
  freer-base, `Cont` is `Free[Shift, A]`, so the `Shift` depth, the
  `Cont.Fuse` budget and the three-shape `/` runner no longer exist.
- specs/jdk17-adaptive-runtime.md cites
  `okay-platform/src/main/scala-jvm/Platform.scala`.
- BUGS.md `growing-stale-route` goes from `reopened` to `wontfix`,
  because it was closed as a trade on 2026-09-18 (8af62bc77).
- backlog `handler-single-pass`: the fusedSWr bar names the 2026-09-27
  re-measure (12.4 us / 122 624 B) beside the 13.7 it was set at.
- Not done here: the specs/schedulers.md `adaptive` row belongs to
  own-managed-blocking, which rewrites it.
