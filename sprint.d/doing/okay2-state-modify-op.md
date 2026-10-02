- [ ] okay2-state-modify-op — the twin of effect-row-recursion-cost's D1
      (specs/effect-row-cost.md). THE OPERATION LANDED in okay2-level1-api
      (2026-10-02): okay2's State is `Get` + `Update(f)`, `modify` is one
      `Update(Modified(f))`, the handlers, zoom and the lexical clauses
      read it. Left: measure bytes a level with the same counting
      recursion as the core, against the get-then-set it replaced.
      (2026-09-26)
