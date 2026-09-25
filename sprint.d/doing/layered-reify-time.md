- [ ] layered-reify-time — PRIORITY: LOW (a measurement owed). monadic-reflection-stacked
      (2026-09-25) switched `Layered.reify` from `push(e.map(η))` to
      `η $ e` (Delim.dollar) on semantics and bytes (−40 B per
      resumption). Its only timing ran at load 60-108, with a VM at
      ~1000% CPU. THE LANE: DelimBenchmark.layered.* gated at load < 4,
      three rounds, in one run (the pair is same-run by design), and
      the ratio in a src/jmh/history.d file and specs/layered-reflection.md Results.
      If dollar is slower by more than the noise, say so there. Push
      and dollar mean the same, so reverting is one line. (2026-09-25)
