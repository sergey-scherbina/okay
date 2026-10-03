- [ ] shift-generator-cost — PRIORITY: MEDIUM, MEASURED 2026-10-03
      (history.d shift-prompt-one-boundary). After a prompt became ONE
      boundary (delimited-simplify), `push`'s own lanes fell (delimPushOnly
      0.55x, delimDollarResume 0.73x) but DelimBenchmark.delimGenerator —
      `shift` per yield, a capture and a resumption each — stayed 1.09x
      master's (76.7 vs 70.5 us) with 18% FEWER bytes (678 KB vs 823 KB).
      So it is not allocation. Candidates: the capture predicate `p.is`
      (a lazy val read and two comparisons, through a megamorphic
      `Function1`), the `Resumption` → `Free.delay(Nested(op(Resume)))` →
      stepped-into road (four indirections a resume), `Next` objects a
      step. Profile before changing (async-profiler cpu, one lane).
