## frege-effects-lists - a Frege stage that performs, Async, lists both ways

okay-frege, second pass. `Frege.stageWith[I, O, F]` is a Frege stage
that also performs operations of `F`. Its row is okay-stream's
effectful stage row, so it composes through `through` like an okay
stage. `Frege.stage` is now `stageWith` at the empty row: `Pure` is
`Nothing`, so the rows are equal and there is one walker instead of two.

`Ops.sleep` brings `Async` to Frege. It answers the milliseconds slept
(an `Operation Long`), because Frege's `()` is a Java `short`.

`Frege.chunks` and `Frege.list` convert Frege lists and okay `Chunks`
lazily in both directions, infinite on either side. A Frege function
over an infinite okay source takes 10 elements, and okay produces at
most one chunk. `list` accepts only pure `Chunks`, because an effectful
source forced inside a Frege thunk would be lazy IO. `Frege.option` and
`Frege.maybe` convert between `Maybe` and `Option`. 24 tests. Two
mutants (an eager list, a sleep that does not sleep) each fail their own
test. Docs: docs/modules/okay-frege.md; specs/frege.md stage 2.
