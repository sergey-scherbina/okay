## okay2-lexical-walk-stacked - the rest of Lexical and Layered in the Scala 2 core

specs/okay2.md stage 48.

- `Lexical.walk` and `Lexical.State.walk` over `Instances` (a resume
  loop, state threaded purely, `Instances.exhausted` at the top; one
  cast where okay2's `Instances.Op` erases the operation).
- `Lexical.Stacked` (`Deep`, `Tail`, and `State` with typed doors): an
  instance holds its `In`, every door takes the stack in force and asks
  `Has`; `Stack.dollarResumed` added for the tail guard.
- `Layered.Stacked.reify[M, R, F](st)(body)` and `m.reflectAt[F](st,
  layer)`: a layer kept past its reify does not compile.
- Every stacked door is a builder (`tail[A, G](st)(s0)(body)`), the
  Scala 2 shape for "write only what cannot be inferred".
- TestLexicalWalk 7, TestLexicalStacked 2, TestLayeredStacked 3 — the
  twins of the Scala 3 suites, values compared instead of Bisim.
