## okay2-layered - layered monadic reflection in the Scala 2 core

specs/okay2.md stage 47; the twin of specs/layered-reflection.md.

- `okay2.Layered`: `Layer[M]` with Option, Either (a type-lambda
  instance) and List, `Reflect[M, R]`, `reify` as `η $ e` on
  `Delim.dollar`, `reflect` as a `shift0` through an implicit class on
  `M[X]`. A body receives its capability (no context functions in
  Scala 2).
- TestLayered (7): every value of the Scala 3 suite — Filinski's law,
  one layer in direct style, both two-layer orders, three layers, and
  the capability leak throwing `NoPrompt`. 63 results with TestLexical
  across the three platforms.
- The stacked layers are filed with the stacked Lexical instances in
  `okay2-lexical-walk-stacked`.
