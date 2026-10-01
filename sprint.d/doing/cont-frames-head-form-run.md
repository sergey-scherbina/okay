- [ ] cont-frames-head-form-run — PRIORITY: LOW. The segmented machine
      hands an operation nobody on its stack answers out as
      `Bind(focus, Run(fs, st))`: one `Run` node per operation of a
      FOREIGN effect running under a delimiter (the guard shape —
      okay-llm's `Cut`, okay-ui's `Scope`). DelimBenchmark's
      `writerTellUnderDelim` reads 1.13x the single-list machine, with
      FEWER bytes than it (270 vs 286 KB/op), after two cuts on the way
      back: `Rev.onto` tests an empty machine first (1.25 -> 1.155x,
      cont-step-on-frames 1l) and a forced `Resume` enters at the
      registers (-> 1.13x, cont-frames-reentry-and-close). What is left
      is time, not bytes: profile the out-and-back (the outer handler,
      `Resume`, `Frames.machine`'s entry) before changing the head form.
