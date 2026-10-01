- [ ] handled-on-control-func — PRIORITY: LOW, operator ask 2026-10-01:
      `Handled` (Staged.scala, Direct.staged's carrier) is `Func` at the
      diagonal with a phantom row, and its monad copied `Control[Func]`'s
      three bodies. Make it delegate to `Control[Func]` — one closure
      monad — keeping the opaque type (the direct macro reads the row
      off it). A/B on StagedBenchmark decides.
