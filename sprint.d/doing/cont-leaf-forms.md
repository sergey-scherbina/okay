- [ ] cont-leaf-forms — PRIORITY: MEDIUM (2026-10-04). Cont's leaf has four
      forms (`Strict`, `Program`, `Lazily`, `Resume`) and `ContMacro` (518
      lines, selective CPS) picks among them — the price of strict against
      lazy `k`. On `Delimited` the lazy `k` is cheaper than it was on λ$:
      measure whether the strict leaf still earns its keep (statePara,
      fib100, contAnswer); if not, the macro shrinks.
