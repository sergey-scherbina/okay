## effect-pages - every effect in the README has its own page with a compiled example

The operator: list the remaining effects briefly, and give each effect a
link to a detailed explanation with examples in a document of its own,
reusing one where it exists.

- `docs/effects/`: thirteen new pages, each what the effect is for, its
  operations and handlers in a table, a worked example and the notes that
  bite: Reader, State, Writer, Throws and Abort (with where Validated and
  Chronicle fit), Maybe, Chronicle, Resource, Async, Supply and Fresh,
  Once, Choice and Logic, Gen, Prob.
- Existing pages reused: Delim links to the continuations book's
  "Prompts" chapter, several instances to `docs/many-instances.md`, your
  own effect to `docs/your-own-effect.md`.
- README's Effects list: sixteen entries, each name a link; Maybe,
  Chronicle, Supply and Fresh, Once, Gen and Prob added in two or three
  lines each. The docs index names the pages.
- Every example line is pinned: `TestDocExamplesEffects` (core, 12 tests)
  and `TestDocExamplesAsync` (okay-platform) hold them verbatim, answer
  comment included, and assert each answer. A result the page shows is a
  `val` there, so the line compiles as it reads. Writing them corrected
  two first drafts: `Prob.runExact` answers joint weights, not
  probabilities (`.posterior` normalizes), and `Supply.run` answers the
  next value with the answer.
