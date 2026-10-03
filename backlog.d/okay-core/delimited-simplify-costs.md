- [ ] delimited-simplify-costs — PRIORITY: MEDIUM, MEASURED 2026-10-04
      (history.d shift-generator-cost). Against cont-atm (6fc400e08), after
      shift-generator-cost: statePara 1.13x (+32 KB), fib100 1.11x (+3 KB),
      stateForeign 1.31x (same bytes). Two causes, measured apart:
      (1) stateForeign — N operations of State inside `Shift.run` under ONE
      reset — is push-one-boundary (ec9ee796a: 41.5-42.9 us; the same tree
      with `push` written back as `dollar(p)(pure)` 31.7-36.4). Per
      operation the two forms do the same work (the boundary is installed
      once a run), so it is the JIT's shape of the loop, not the machine's
      work: PrintInlining shows `go` (1025 B) never inlined in either, and
      the profile moves time into the benchmark's own `boxToInteger`
      (1.5% -> 26%). Read under hsdis before changing anything.
      (2) statePara/fib100 — the strict `k` of Cont — the one loop answers
      `Free[F, Z]`, so a forced `k` allocates a `Return` for its answer, and
      the `Kont` is called virtually (`Segment`, `Held`). A loop for a
      machine ALONE that answers `Z` itself would take both back, at the
      price of a second copy of `go` — the copy delimited-simplify deleted.
