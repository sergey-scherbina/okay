## effect-row-recursion-cost - a counter in an effect row stops paying twice

- `State.modify` is ONE operation now, `State.Modify(f)`, where it used
  to be a `get` then a `set`. Every interpreter of State's operations
  handles it: `State.handle`, `zoom`, `Bisim.Answers.state`, Lexical's
  deep, shallow, tail and walk clauses (and `Inst.modify`), the three
  stagers, and the test handlers.
- Measured exact, bytes a level of a counting recursion
  (`ProbeRowCost`): the row as written, `State.run(Writer.run(p))`,
  384 -> 168; State alone, 240 -> 96.
- The guide says the handler order is also a price: a forwarded
  operation costs 72 B, so the frequent effect goes innermost.
  `Writer.run(State.handle(p))` reads 96 B a level where
  `State.run(Writer.run(p))` reads 168.
- specs/effect-row-cost.md has the decomposition, the decisions and
  the deferred D2. The okay2 twin is filed (okay2-state-modify-op).
