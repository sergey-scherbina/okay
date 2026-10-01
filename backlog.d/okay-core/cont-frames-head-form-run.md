- [ ] cont-frames-head-form-run — PRIORITY: MEDIUM. The segmented
      machine hands an operation nobody on its stack answers out as
      `Bind(focus, Run(fs, st))`: one `Run` node per operation of a
      FOREIGN effect running under a delimiter (the guard shape —
      okay-llm's `Cut`, okay-ui's `Scope`). DelimBenchmark's
      `writerTellUnderDelim` reads 1.155x the single-list machine
      (+8 B/op) after cont-step-on-frames 1l moved the empty-machine
      re-entry first in `Rev.onto` (it was 1.25x). Ask: can the head
      form's continuation avoid the `Run` when the live segment is one
      frame, or when `st` is the run's root delimiter; and what is left
      of the 1.155x once it does (profile the out-and-back: `Resume`,
      `Frames.run`'s entry, `onto`). history.d 2026-10-01T053245Z.
