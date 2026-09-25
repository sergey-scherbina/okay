## lexical-tail-allocs - Lexical pays as it goes: the row decides the guard; a map that cost 72 B found and removed

- `Inst[F, G]` works over the whole row the body uses. `deep` and
  `shallow` demand `Delim` in it (a Writer-only row does not compile for
  them). `tail` picks its closing at compile time: a row without `Delim`
  gets no guard and no machine, and the program is `(S, A) ! G`, run with
  `!.run` or any other runner. `Delim + G` is read as `G` through a witness
  built without a cast.
- FOUND BY MEASURING: the unguarded path first cost MORE than the guarded
  one (438 304 against 366 656 B per 1000 get/set). The same program run in
  the Delim machine cost the same, so the runner was ruled out. The cause
  was `body.map(finish)`, whose root `Bind` made `resume` re-associate
  every step. The close now walks the body like `State.handle`: 366 224.
- REFUTED and reverted: a state cell in place of the clause's `(S, X)`
  pair. Escape analysis already removed the pair.
- Result: tail is 1.65x the row's bytes, the remainder being per-operation
  laziness. The row stays the zero-overhead default for one handler of a
  kind. Backlog lexical-tagged-walk has the design that could close the
  rest, and why it is not the default.
- TestLexicalPayAsYouGo (4). The earlier Lexical suites were moved to the
  whole-row types.
