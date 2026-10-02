- [ ] docs-level1-language — level 1, with handle-values-rest (operator,
      2026-10-02). guide.md, tutorial.md and docs/effects/* still teach
      through the old runners (`State.run(5)(p)`, `runEither`). Move them to
      `p.handle(State(5))` and `Handler[F] { … }`, every changed example
      line pinned by its test. The old runners stay, not in the lead.
      (2026-10-02)
