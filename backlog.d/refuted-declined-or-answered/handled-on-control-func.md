- handled-on-control-func — REFUTED 2026-10-01 (operator ask, measured):
  `Handled` (Direct.staged's carrier, `Func` on its diagonal) delegating
  its shift/pure/run/monad to `Control[Func]` instead of its own three
  bodies. StagedBenchmark stagedDirect 1.75x, stagedHand 1.70x, bytes
  85 -> 147 KB/op: through `Control[Func]`'s given the closures stop
  inlining the way the copied bodies do, and Direct.staged's win IS that
  inlining. The code is parked on feature/handled-on-control-func
  (e1e692bc1). Also found the same day: `Control[Func]` cannot carry the
  codecs' walks — a Func-carried walk overflows on a FLAT array of
  100 000 elements (every bind a nested call; the native walks loop over
  siblings), so the native + Cont pairs in okay-codec stay.
