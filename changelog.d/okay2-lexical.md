## okay2-lexical - handler instances as prompts in the Scala 2 core, on `dollarResumed`

specs/okay2.md stage 46; the twin of specs/lexical-instances.md.

- `Delim.dollarResumed` and `Shots` in the okay2 machine first (the
  `Dollar` op and the `Ret` frame carry the count, `retake` copies a
  `Ret` with a fresh count per capture, the `Dollar` step bumps it),
  which the tail guard needs.
- `okay2.Lexical`: `Inst`, `Clauses`/`ShallowClauses`/`TailClauses`,
  `deep`, `shallow`, `tail` (guarded), `tailPure` (the walk close),
  `handle` by clause kind, `MultiShotAcrossTail`; `Lexical.State` with
  every strategy and typed `get`/`set`/`put` doors.
- TestLexical (15): every value of the Scala 3 TestLexical and
  TestLexicalTail, multi-shot through a raw outer `Delim.shift` where
  the Scala 3 suite uses `Layered`. 99 results with the dollar suites.
- The Scala 2 limits, recorded: rows written in full (`Delim + G`), two
  tail names instead of a `Closing` given, and one cast where Scala 2
  cannot refine a State operation's answer type (the Flip user clause
  carries one per answer, said in the test). `walk` and the stacked
  instances filed as `okay2-lexical-walk-stacked`.
