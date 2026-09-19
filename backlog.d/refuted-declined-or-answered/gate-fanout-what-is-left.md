- gate-fanout-what-is-left — MEASURED 2026-09-19: sampled the process
  tree every 3s during one quiet full-family `scripts/gate.sh` (bound
  already landed, `gate-bound-test-fanout`), keyed on descendants of
  the gate's own PID via BFS over pid/ppid (not a `^node$` name match
  — that was the earlier cut's own mistake, misattributing a
  sibling's build). Peak: 129 descendants (rows counted include the
  gate's own bash/sbt ancestry). Breakdown at the peak sample:
  64 `node` (Scala.js test runners — the family cross-compiles
  JVM/JS/Native, one node process per JS test suite), 18 `(clang)`
  (Scala Native linking), and ~50 spread thin across `okay-*-test`
  native test binaries (1-3 each, one per Native module's own test
  process). No single "mystery" component — it is the natural
  fallout of a JVM+JS+Native cross-build's own three backends running
  their own per-module runners/linkers at once, not link steps or a
  gtk test or anything unaccounted for. No remedy proposed; the
  `parallelExecution := false` bound already landed is what there is
  to do about it short of serializing across backends too, which
  would cost wall-clock for no defect fixed.
