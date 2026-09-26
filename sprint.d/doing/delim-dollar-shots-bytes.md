- [ ] delim-dollar-shots-bytes — every plain `$` pays +16 B for the
      guard it does not use: `Segs.Ret` and `Delim.Dollar` each carry a
      `Shots` field that is null unless the dollar is guarded
      (lexical-tail-guard-abort, history.d: delimDollarOnly 388 008 vs
      372 008 B/op over 1000 dollars). THE LANE: give the guarded dollar
      its own node (or subclass) so the plain one carries no field;
      no cast. DONE WHEN: delimDollarOnly is back to 372 008 B/op,
      stateLexTail and delimGenerator unchanged, the guard's tests
      green. (2026-09-26, found by the continuations perf review)
