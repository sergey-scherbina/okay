- [ ] okay2-state-modify-op — the twin of effect-row-recursion-cost's D1
      (specs/effect-row-cost.md): okay2's `State.modify` is still a get
      then a set. Give okay2's State a one-step `Modify` operation and
      teach its handlers and interpreters the case, the way the Scala 3
      core did (State.handle, zoom, Bisim-style answers, the lexical
      clauses); measure bytes a level with the same counting recursion.
      (2026-09-26)
