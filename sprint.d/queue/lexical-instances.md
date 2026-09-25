- [ ] lexical-instances — specs/lexical-instances.md. Operator 2026-09-25: effect
      instances as prompts (POPL 2020 lexically scoped handlers), with
      EVERY strategy an explicit primitive (row / deep / shallow / tail),
      a default combinator built from them later, and the manual choice
      kept. Stage 0: Lexical.Inst, Clauses, deep/shallow combinators,
      Lexical.State, the two-instance and no-accidental-handling tests.
      Stages 1-3: tail (evidence passing), stacked instances, the default
      combinator. Closes the "fresh prompt" route of docs/many-instances.md
      and is the lane effect-instances-tunnelling asked for. (2026-09-25)
      STAGE 0 LANDED 2026-09-25 (Lexical.Inst/Clauses/deep/shallow,
      Lexical.State, TestLexical 7). Next: stage 1 `tail` (evidence
      passing, priced against row and deep), stage 2 stacked instances,
      stage 3 the default combinator.
