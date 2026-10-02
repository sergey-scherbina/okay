## state-get-update - State is Get and Update

The operator, 2026-10-02: "оставь у State только Update и Get".

- `Set(s)` and `Modify(f)` are gone from the signature. `State.set(s)` is
  `Update(Put(s))` and `State.modify(f)` is `Update(Modified(f))`, their
  signatures unchanged. `Put` and `Modified` are transitions as DATA: two
  sets of one value are equal operations (Bisim compares with `==`), and
  they print as `Update(Put(5))`.
- Every handler over State moved to the two operations: `State.handle`,
  `zoomWith`, Lexical's instances, Bisim (a `Put` is answered once, not
  once per sample), Staged's stagers, the tests and benchmarks, and the
  docs that quote them. TestHandlersAsDollar's MUTANT is rebuilt on
  `Update`.
- State is now writable in the case form (`{ case (s, Get()) => …; case (s,
  Update(g)) => … }`).
- Cost: a write is 1.14x of the old Set node (a Put and a pair, +16 B).
  Backlog `state-write-cost`.
