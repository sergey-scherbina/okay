- [ ] indexed-effects-9-lexical-typed — stage 9 of
      specs/indexed-effects.md: Lexical's clauses over the program type
      rather than the row — `Ops[F, R, P[_]]` with `op[X](e: F[X], k: X
      => P[R]): P[R]`, `Clauses`, `ShallowClauses` the same, so a stacked
      instance's clauses are written over `Under[G, *, St]` and
      `Delim.Stacked.erase` is no longer needed on that road (kept for
      whoever else wants it). The unstacked instances keep `P = [A] =>>
      A ! Delim + G`. Every clause implementation in the library and the
      tests moves; the doc examples pinned. Changes existing API: full
      `affected master staged`.
