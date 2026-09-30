## indexed-effects-8-prog-facade — the `Prog` facade removed, `okay.sql.Tx` is the data road

`okay.Prog` (freer-base stage 2's opaque phantom-indexed facade over
`Free`) is deleted: its two consumers already ran on the indexed tree
(`Delim.Stacked` typed since stage 4, `Tx.Data` since stage 2).
`Tx.Data.begin/commit/rollback/update/batch/describe/async/interpret`
are now `Tx.begin` etc.; the facade class `Tx(db)`, `Tx.Step`, `Tx.run`
and TestTx are gone (its shapes were TestTxData's). TestProg keeps the
stacked shapes. docs/guide.md's typestate section is rewritten over
`Tx`, docs/theory/03 names the indexed signature as the third
paramonad instance. specs/indexed-effects.md stage 8 Results.
Commits: see `git log --grep indexed-effects-8`.
