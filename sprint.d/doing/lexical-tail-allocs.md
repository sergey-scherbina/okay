- [ ] lexical-tail-allocs — PRIORITY: LOW (perf; measured gap). `Lexical.tail` costs 1.65x
      the row handler's bytes (366 590 vs 222 040 B per 1000 get/set,
      lexical-instances 2026-09-25): +72 B per operation, from the
      `Free.delay` thunk around the cell access, the `(S, X)` pair from
      `TailClauses.op`, the polymorphic `run`, and a `Return`. Candidates,
      each measured on its own (performance skill): a `TailClauses` shape
      that writes the state through a setter instead of returning a
      pair; an `Inst` subclass per strategy with `perform` as a plain
      method (no polymorphic function value); and whether the thunk is
      needed at all when the program is never re-run concurrently. DONE
      WHEN the row/tail byte ratio is recorded after each, kept or
      refuted. (2026-09-25)
