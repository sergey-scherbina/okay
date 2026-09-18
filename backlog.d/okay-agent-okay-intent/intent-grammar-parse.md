- [ ] intent-grammar-parse — intent by GRAMMAR over `okay-lex` and
      `okay-parse`, the way `Temporal` does dates: deterministic,
      explainable, and refusing rather than guessing. Expensive in
      rules, and the honest reason to want it is a domain where a wrong
      answer is worse than no answer.
      GATED 2026-09-07: no consumer has named a domain where a wrong
      answer is worse than no answer; the model tier at 0.909 macro
      F1 with zero undecodable replies, the slot parsers (when,
      duration, people, amount) already refusing rather than guessing
      where a wrong value would act. Opens with that consumer.
