- [ ] handle-overloads-chain — PRIORITY: LOW (core-simplify review,
      2026-10-02). Free.scala has `p.handle(h)`, `p.handle(h1, h2)` and
      `p.handle(h1, h2, h3)`, the latter two documented as exactly
      `p.handle(h1).handle(h2)…`, each carrying its own copy of the
      row/`Distinct`/`N` evidence (4, 8 and 12 using-parameters). If the
      chained form infers at every current call site — check by
      deleting the two and compiling every module's tests
      (`affected master Test/compile` is not enough: grep the callers
      across all modules) — two of the three overloads go. If some
      site needs the overload for inference, record which and why, and
      keep them.
