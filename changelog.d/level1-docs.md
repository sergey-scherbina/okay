## level1-docs - the three levels written down, and level 1's page

Level 1 of the API, step 5 (operator, 2026-10-02).

- specs/api-levels.md: who sees what. Level 1 is the user, level 2 the
  effect author, level 3 the library. Level 1's surface in one table, by
  default and through `Effects[M]`, with the decisions behind it.
- docs/effects-and-continuations.md: the user's whole API on one page,
  `A ! F` and `pure`, `perform`, `shift`/`shift0`, `reset`, `handle`,
  `run`. It covers effects alone, continuations alone, the two mixed,
  two answer types in one program, the direct style with and without
  marks, the typeclass in Free and Eager, the measured costs and the
  literature. Linked first from docs/README.md.
- Every example line is pinned by TestDocExamplesLevel1 (6) and
  TestDocExamplesLevel1Direct (2), which call `perform(op)` both ways: the
  extension `op.perform` is the function `perform(op)`, so no second
  definition was added (one would be E120).
